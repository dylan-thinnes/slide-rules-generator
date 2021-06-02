{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- default
import Data.Default

-- lens
import Control.Lens.Combinators hiding (each)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)

-- mtl
import Control.Monad.State

-- pipes
import Pipes
import qualified Pipes.Prelude as PP

-- local (sliderules)
import SlideRules.Lenses
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

type Generator = ListT (State GenState)

type TickCreator = InternalFloat -> TickInfo

data GenState = GenState
    { _preTransformations  :: [Transformation]
    , _postTransformations :: [Transformation]
    , _tickCreator         :: TickCreator
    , _out                 :: S.Seq Tick
    , _logging             :: S.Seq String
    }
    -- deriving (Show)

makeLenses ''GenState

generate :: Generator a -> GenState
generate act = generateWith act def

generateWith :: Generator a -> GenState -> GenState
generateWith act = execState (runListT act)

summarize :: Generator a -> [(String, InternalFloat, InternalFloat)]
summarize = foldMap summarize1 . _out . generate
    where
        summarize1 tick =
            case tick ^. info . mlabel of
                Nothing -> []
                Just label -> [(label ^. text, tick ^. prePos, tick ^. postPos)]

calculate :: InternalFloat -> GenState -> Maybe (InternalFloat, InternalFloat)
calculate x s = do
    _prePos <- runTransformations (_preTransformations s) x
    _postPos <- runTransformations (_postTransformations s) _prePos
    pure (_prePos, _postPos)

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = do
    (_prePos, _postPos) <- calculate x s
    let _info = _tickCreator s _prePos
    pure $ Tick { _info, _prePos, _postPos }

instance Default GenState where
    def = GenState [] [] (const def) (S.fromList []) (S.fromList [])

together :: [Generator a] -> Generator a
together = join . Select . each

list :: [a] -> Generator a
list = Select . each

withPrevious :: Lens' GenState a -> (a -> a) -> Generator b -> Generator b
withPrevious lens f action = do
    previous <- use lens
    Right res <- together
        [ fmap Left $ lens %= f
        , fmap Right action
        , fmap Left $ lens .= previous
        ]
    return res

preTransform :: Transformation -> Generator a -> Generator a
preTransform transformation = withPrevious preTransformations (transformation :)

postTransform :: Transformation -> Generator a -> Generator a
postTransform transformation = withPrevious postTransformations (transformation :)

translate :: InternalFloat -> InternalFloat -> Generator a -> Generator a
translate offset scale = preTransform (Offset offset) . preTransform (Scale scale)

withTickCreator :: ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo) -> Generator a -> Generator a
withTickCreator handlerF = withPrevious tickCreator handlerF

fromInfoX :: (TickInfo -> InternalFloat -> TickInfo) -> ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo)
fromInfoX handlerF = \f x -> handlerF (f x) x

withInfoX :: (TickInfo -> InternalFloat -> TickInfo) -> Generator a -> Generator a
withInfoX handlerF = withTickCreator (fromInfoX handlerF)

fromInfo :: (TickInfo -> TickInfo) -> ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo)
fromInfo handlerF = fromInfoX (\info _ -> handlerF info)

withInfo :: (TickInfo -> TickInfo) -> Generator a -> Generator a
withInfo handlerF = withTickCreator (fromInfo handlerF)

output :: InternalFloat -> Generator ()
output x = do
    Just tick <- gets $ genTick x
    out <>= S.fromList [tick]

saveToLog :: String -> Generator ()
saveToLog s = logging <>= S.fromList [s]

measure :: InternalFloat -> InternalFloat -> Generator (InternalFloat, InternalFloat)
measure a b = do
    Just (preA, postA) <- gets (calculate a)
    Just (preB, postB) <- gets (calculate b)
    return (preB - preA, postB - postA)
