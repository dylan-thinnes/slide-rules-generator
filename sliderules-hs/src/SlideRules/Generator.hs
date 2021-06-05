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
import Control.Monad.Reader
import Control.Monad.List

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

type Generator = ListT (ReaderT Settings (State GenState))

type TickCreator = InternalFloat -> TickInfo

data Settings = Settings
    { tolerance :: InternalFloat
    }

data GenState = GenState
    { _preTransformations      :: [Transformation]
    , _postTransformations     :: [Transformation]
    , _postPostTransformations :: [Transformation]
    , _tickCreator             :: TickCreator
    , _out                     :: S.Seq Tick
    , _logging                 :: S.Seq String
    }
    -- deriving (Show)

makeLenses ''GenState

generate :: Settings -> Generator a -> GenState
generate settings act = generateWith settings act def

generateWith :: Settings -> Generator a -> GenState -> GenState
generateWith settings act = execState $ runReaderT (runListT act) settings

summarize :: Settings -> Generator a -> [(String, InternalFloat, InternalFloat)]
summarize settings = foldMap summarize1 . _out . generate settings
    where
        summarize1 tick =
            case tick ^. info . mlabel of
                Nothing -> []
                Just label -> [(label ^. text, tick ^. prePos, tick ^. postPos)]

calculate :: InternalFloat -> GenState -> Maybe (InternalFloat, InternalFloat, Maybe InternalFloat)
calculate x s = do
    _prePos <- runTransformations (_preTransformations s) x
    _postPos <- runTransformations (_postTransformations s) _prePos
    let _postPostPos = runTransformations (_postPostTransformations s) _postPos
    pure (_prePos, _postPos, _postPostPos)

genTick :: InternalFloat -> GenState -> Maybe Tick
genTick x s = do
    (_prePos, _postPos, _postPostPos) <- calculate x s
    let _info = _tickCreator s _prePos
    pure $ Tick { _info, _prePos, _postPos, _postPostPos }

instance Default GenState where
    def = GenState [] [] [] (const def) (S.fromList []) (S.fromList [])

list :: [a] -> Generator a
list xs = ListT $ pure xs

together :: [Generator a] -> Generator a
together = join . list

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

postPostTransform :: Transformation -> Generator a -> Generator a
postPostTransform transformation = withPrevious postPostTransformations (transformation :)

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

-- Do not show postPostPos here - it should not be visible
measure :: InternalFloat -> InternalFloat -> Generator (InternalFloat, InternalFloat)
measure a b = do
    Just (preA, postA, _) <- gets (calculate a)
    Just (preB, postB, _) <- gets (calculate b)
    return (preB - preA, postB - postA)
