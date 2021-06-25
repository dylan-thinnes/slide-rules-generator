{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SlideRules.Scales where

-- base
import Data.Foldable (fold)
import Data.List (nub)
import Data.Maybe (isJust)
import GHC.Generics

-- containers
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set

-- deepseq
import Control.DeepSeq

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- linear
import Linear.V2

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils
import SlideRules.Renderer
import qualified SlideRules.Tick as Tick

-- SCALE SPECS

data ScaleID = IID Integer | SID String
    deriving (Eq, Ord, Show, Generic)

instance NFData ScaleID

data ScaleSpec fl = ScaleSpec
    { baseTolerance :: fl
    , tickIdentifier :: fl -> [(fl, ScaleID)]
    , generator :: Generator fl ()
    , offsetter :: Offsetter fl
    , renderSettings :: RenderSettings fl
    }

unitRadius :: (Fractional fl, Floating fl) => fl -> Offsetter fl
unitRadius r = Radial $ \_ -> r / 2 / pi

unitArchimedes :: (Fractional fl, Floating fl, Num fl) => fl -> fl -> Offsetter fl
unitArchimedes r angle = Radial $ \x -> angle * x + r / 2 / pi

incline :: (Num fl) => fl -> Offsetter fl
incline s = Vertical (s *)

noOffset :: Num fl => Offsetter fl
noOffset = Vertical $ const 0

-- GENERATE SCALES

generateScales :: (Num fl, Default fl) => ScaleSpec fl -> M.Map ScaleID (S.Seq (Tick fl))
generateScales ScaleSpec {..} =
    generateScales'
        tickIdentifier
        (Settings baseTolerance offsetter)
        generator


generateScales' :: (Num fl, Default fl) => (fl -> [(fl, ScaleID)]) -> Settings fl -> Generator fl a -> M.Map ScaleID (S.Seq (Tick fl))
generateScales' tickIdentifiers settings generator =
    let ticks = generateTicksOnly settings generator
        insertTick tick map =
            foldr
                (\(x', id) -> M.insertWith (<>) id (S.singleton $ tick { _postPos = x' }))
                map
                (tickIdentifiers $ _postPos tick)
        identifiedTicks = foldr insertTick M.empty ticks
    in
    identifiedTicks

generateTicksOnly :: (Num fl, Default fl) => Settings fl -> Generator fl a -> [Tick fl]
generateTicksOnly settings = Set.toList . fst . unlogging . generate settings

-- TICK IDENTIFIERS

defaultIdentifier :: fl -> [(fl, ScaleID)]
defaultIdentifier x = [(x, SID "default")]

floorIdentifier :: (Ord fl, RealFrac fl) => fl -> (Integer, Integer) -> fl -> [(fl, ScaleID)]
floorIdentifier leeway (lower, upper) x =
    let iids = nub $ map floor [x - leeway, x, x + leeway]
    in
    [ (x - fromIntegral iid, IID iid)
    | iid <- iids
    , iid >= lower
    , iid < upper
    ]

-- RENDERING

renderScales :: (Num fl, Default fl, RealFrac fl, Renderer a fl, Monoid (Representation a fl)) => Proxy (a, fl) -> ScaleSpec fl -> Representation a fl
renderScales proxya spec@ScaleSpec{ renderSettings } =
    renderTicks proxya renderSettings $ (foldMap . foldMap) (:[]) (generateScales spec)

writeScalesToFile :: (Num fl, Default fl, RealFrac fl, Renderer a fl, Monoid (Representation a fl)) => Proxy (a, fl) -> FilePath -> ScaleSpec fl -> IO ()
writeScalesToFile proxya filepath spec =
    writeRepToFile proxya filepath (renderScales proxya spec)

