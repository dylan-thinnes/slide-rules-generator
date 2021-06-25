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

-- mtl
import Control.Monad.RWS.Strict

-- local (sliderules)
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

type Generator fl = RWS (Settings fl) (Logging fl) (GenState fl)

type TickCreator fl = fl -> TickInfo fl

data Settings fl = Settings
    { tolerance :: fl
    , calcOffset :: Offsetter fl
    }

data GenState fl = GenState
    { _preTransformations  :: [Transformation fl]
    , _postTransformations :: [Transformation fl]
    , _tickCreator         :: TickCreator fl
    }
    -- deriving (Show)

newtype Logging fl = Logging { unlogging :: (Set.Set (Tick fl), S.Seq String) }
    deriving (Semigroup, Monoid)

instance (Num fl, Default fl) => Default (GenState fl) where
    def = GenState
        { _preTransformations = []
        , _postTransformations = []
        , _tickCreator = const def
        }

makeLenses ''GenState

generate :: (Num fl, Default fl) => Settings fl -> Generator fl a -> Logging fl
generate settings act = generateWith settings act def

generateWith :: (Num fl, Default fl) => Settings fl -> Generator fl a -> GenState fl -> Logging fl
generateWith settings act = snd . evalRWS act settings

summarize :: (Num fl, Default fl) => Settings fl -> Generator fl a -> [(String, fl, fl)]
summarize settings = foldMap summarize1 . fst . unlogging . generate settings
    where
        summarize1 tick =
            case tick ^. info . mlabel of
                Nothing -> []
                Just label -> [(label ^. text, tick ^. prePos, tick ^. postPos)]

calculate :: (Ord fl, Num fl, Floating fl, Fractional fl) => fl -> Settings fl -> GenState fl -> Maybe (TickF fl ())
calculate x settings genState = do
    _prePos <- runTransformations (_preTransformations genState) x
    _postPos <- runTransformations (_postTransformations genState) _prePos
    let _offset = applyOffsetter (calcOffset settings) _postPos
    pure $ Tick { _prePos, _postPos, _offset, _info = () }

genTick :: (Ord fl, Num fl, Floating fl, Fractional fl) => fl -> Settings fl -> GenState fl -> Maybe (Tick fl)
genTick x settings genState = do
    tick@Tick { _prePos } <- calculate x settings genState
    let _info = _tickCreator genState _prePos
    pure $ tick { _info }

withPrevious :: Ord fl => Lens' (GenState fl) a -> (a -> a) -> Generator fl b -> Generator fl b
withPrevious lens f action = do
    previous <- use lens
    lens %= f
    res <- action
    lens .= previous
    return res

preTransform :: (Ord fl, Num fl, Floating fl, Fractional fl) => Transformation fl -> Generator fl a -> Generator fl a
preTransform transformation = withPrevious preTransformations (transformation :)

postTransform :: (Ord fl, Num fl, Floating fl, Fractional fl) => Transformation fl -> Generator fl a -> Generator fl a
postTransform transformation = withPrevious postTransformations (transformation :)

translate :: (Ord fl, Num fl, Floating fl, Fractional fl) => fl -> fl -> Generator fl a -> Generator fl a
translate offset scale = preTransform (Offset offset) . preTransform (Scale scale)

withTickCreator :: Ord fl => ((fl -> TickInfo fl) -> fl -> TickInfo fl) -> Generator fl a -> Generator fl a
withTickCreator handlerF = withPrevious tickCreator handlerF

fromInfoX :: Ord fl => (TickInfo fl -> fl -> TickInfo fl) -> ((fl -> TickInfo fl) -> fl -> TickInfo fl)
fromInfoX handlerF = \f x -> handlerF (f x) x

withInfoX :: Ord fl => (TickInfo fl -> fl -> TickInfo fl) -> Generator fl a -> Generator fl a
withInfoX handlerF = withTickCreator (fromInfoX handlerF)

fromXInfo :: Ord fl => (fl -> TickInfo fl -> TickInfo fl) -> ((fl -> TickInfo fl) -> fl -> TickInfo fl)
fromXInfo handlerF = fromInfoX (flip handlerF)

withXInfo :: Ord fl => (fl -> TickInfo fl -> TickInfo fl) -> Generator fl a -> Generator fl a
withXInfo handlerF = withTickCreator (fromXInfo handlerF)

fromInfo :: Ord fl => (TickInfo fl -> TickInfo fl) -> ((fl -> TickInfo fl) -> fl -> TickInfo fl)
fromInfo handlerF = fromXInfo (const handlerF)

withInfo :: Ord fl => (TickInfo fl -> TickInfo fl) -> Generator fl a -> Generator fl a
withInfo handlerF = withTickCreator (fromInfo handlerF)

output :: (Ord fl, Num fl, Floating fl, Fractional fl) => fl -> Generator fl ()
output x = runMayFail_ $ do
    Just tick <- asksGets $ genTick x
    tell $ Logging (Set.singleton tick, mempty)

saveToLog :: Ord fl => String -> Generator fl ()
saveToLog msg = tell $ Logging (mempty, S.singleton msg)

withs :: [Generator fl a -> Generator fl a] -> Generator fl a -> Generator fl a
withs = foldr (.) id

-- Do not show postPostPos here - it should not be visible
measure :: (Ord fl, Num fl, Floating fl, Fractional fl) => fl -> fl -> Generator fl (Maybe (fl, fl, fl))
measure a b = runMayFail $ do
    Just (tickA@Tick { _prePos = preA, _postPos = postA, _offset = radA }) <- asksGets $ calculate a
    Just (tickB@Tick { _prePos = preB, _postPos = postB, _offset = radB }) <- asksGets $ calculate b
    pure (preB - preA, postB - postA, truePos tickB - truePos tickA)
