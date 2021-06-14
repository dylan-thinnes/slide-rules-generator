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

type Generator = ReaderT Settings (State GenState)

type TickCreator = InternalFloat -> TickInfo

data Settings = Settings
    { tolerance :: InternalFloat
    , calcRadius :: Maybe (InternalFloat -> InternalFloat)
    }

data GenState = GenState
    { _preTransformations      :: [Transformation]
    , _postTransformations     :: [Transformation]
    , _tickCreator             :: TickCreator
    , _out                     :: S.Seq Tick
    , _logging                 :: S.Seq String
    }
    -- deriving (Show)

instance Default GenState where
    def = GenState
        { _preTransformations = []
        , _postTransformations = []
        , _tickCreator = const def
        , _out = S.fromList []
        , _logging = S.fromList []
        }

makeLenses ''GenState

generate :: Settings -> Generator a -> GenState
generate settings act = generateWith settings act def

generateWith :: Settings -> Generator a -> GenState -> GenState
generateWith settings act = execState $ runReaderT act settings

summarize :: Settings -> Generator a -> [(String, InternalFloat, InternalFloat)]
summarize settings = foldMap summarize1 . _out . generate settings
    where
        summarize1 tick =
            case tick ^. info . mlabel of
                Nothing -> []
                Just label -> [(label ^. text, tick ^. prePos, tick ^. postPos)]

calculate :: InternalFloat -> Settings -> GenState -> Maybe (TickF ())
calculate x settings genState = do
    _prePos <- runTransformations (_preTransformations genState) x
    _postPos <- runTransformations (_postTransformations genState) _prePos
    let _radius = calcRadius settings <*> pure _postPos
    pure $ Tick { _prePos, _postPos, _radius, _info = () }

genTick :: InternalFloat -> Settings -> GenState -> Maybe Tick
genTick x settings genState = do
    tick@Tick { _prePos } <- calculate x settings genState
    let _info = _tickCreator genState _prePos
    pure $ tick { _info }

withPrevious :: Lens' GenState a -> (a -> a) -> Generator b -> Generator b
withPrevious lens f action = do
    previous <- use lens
    lens %= f
    res <- action
    lens .= previous
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

fromXInfo :: (InternalFloat -> TickInfo -> TickInfo) -> ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo)
fromXInfo handlerF = fromInfoX (flip handlerF)

withXInfo :: (InternalFloat -> TickInfo -> TickInfo) -> Generator a -> Generator a
withXInfo handlerF = withTickCreator (fromXInfo handlerF)

fromInfo :: (TickInfo -> TickInfo) -> ((InternalFloat -> TickInfo) -> InternalFloat -> TickInfo)
fromInfo handlerF = fromXInfo (const handlerF)

withInfo :: (TickInfo -> TickInfo) -> Generator a -> Generator a
withInfo handlerF = withTickCreator (fromInfo handlerF)

output :: InternalFloat -> Generator ()
output x = runMayFail_ $ do
    Just tick <- asksGets $ genTick x
    out <>= S.fromList [tick]

saveToLog :: String -> Generator ()
saveToLog s = logging <>= S.fromList [s]

withs :: [Generator a -> Generator a] -> Generator a -> Generator a
withs = foldr (.) id

-- Do not show postPostPos here - it should not be visible
measure :: InternalFloat -> InternalFloat -> Generator (Maybe (InternalFloat, InternalFloat, InternalFloat))
measure a b = runMayFail $ do
    Just (tickA@Tick { _prePos = preA, _postPos = postA, _radius = radA }) <- asksGets $ calculate a
    Just (tickB@Tick { _prePos = preB, _postPos = postB, _radius = radB }) <- asksGets $ calculate b
    pure (preB - preA, postB - postA, truePos tickB - truePos tickA)
