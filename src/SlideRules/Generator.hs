{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
module SlideRules.Generator where

-- base
import qualified Data.Sequence as S

-- containers
import qualified Data.Set as Set

-- default
import Data.Default

-- lens
import Control.Lens.Combinators hiding (each)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

type Generator a = (Settings, GenState) -> (a, Logging)

type TickCreator = InternalFloat -> TickInfo

data Settings = Settings
    { tolerance :: InternalFloat
    , calcOffset :: Offsetter
    }

data GenState = GenState
    { _preTransformations  :: [Transformation]
    , _postTransformations :: [Transformation]
    , _tickCreator         :: TickCreator
    }
    -- deriving (Show)

newtype Logging = Logging { unlogging :: (Set.Set Tick, S.Seq String) }
    deriving (Semigroup, Monoid)

instance Default GenState where
    def = GenState
        { _preTransformations = []
        , _postTransformations = []
        , _tickCreator = const def
        }

makeLenses ''GenState

generate :: Settings -> Generator a -> Logging
generate settings act = generateWith settings act def

generateWith :: Settings -> Generator a -> GenState -> Logging
generateWith settings act state = snd $ act (settings, state)

summarize :: Settings -> Generator a -> [(String, InternalFloat, InternalFloat)]
summarize settings = foldMap summarize1 . fst . unlogging . generate settings
    where
        summarize1 tick =
            case tick ^. info . mlabel of
                Nothing -> []
                Just label -> [(label ^. text, tick ^. prePos, tick ^. postPos)]

calculate :: InternalFloat -> Settings -> GenState -> Maybe (TickF ())
calculate x settings genState = do
    _prePos <- runTransformations (_preTransformations genState) x
    _postPos <- runTransformations (_postTransformations genState) _prePos
    let _offset = applyOffsetter (calcOffset settings) _postPos
    pure $ Tick { _prePos, _postPos, _offset, _info = () }

genTick :: InternalFloat -> Settings -> GenState -> Maybe Tick
genTick x settings genState = do
    tick@Tick { _prePos } <- calculate x settings genState
    let _info = _tickCreator genState _prePos
    pure $ tick { _info }

withPrevious :: Lens' GenState a -> (a -> a) -> Generator b -> Generator b
withPrevious lens f action (settings, genState) =
    action (settings, lens %~ f $ genState)

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
output x (settings, genState) =
    case genTick x settings genState of
        Nothing -> ((), mempty)
        Just tick -> ((), Logging (Set.singleton tick, mempty))

saveToLog :: String -> Generator ()
saveToLog msg _ = ((), Logging (mempty, S.singleton msg))

withs :: [Generator a -> Generator a] -> Generator a -> Generator a
withs = foldr (.) id
