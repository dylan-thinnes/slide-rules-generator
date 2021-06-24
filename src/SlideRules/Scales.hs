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

data ScaleSpec = ScaleSpec
    { baseTolerance :: InternalFloat
    , tickIdentifier :: InternalFloat -> [(InternalFloat, ScaleID)]
    , generator :: Generator ()
    , offsetter :: Offsetter
    , renderSettings :: RenderSettings
    }

unitRadius :: InternalFloat -> Offsetter
unitRadius r = Radial $ \_ -> r / 2 / pi

unitArchimedes :: InternalFloat -> InternalFloat -> Offsetter
unitArchimedes r angle = Radial $ \x -> angle * x + r / 2 / pi

incline :: InternalFloat -> Offsetter
incline s = Vertical (s *)

noOffset :: Offsetter
noOffset = Vertical $ const 0

-- GENERATE SCALES

generateScales :: ScaleSpec -> M.Map ScaleID (S.Seq Tick)
generateScales ScaleSpec {..} =
    generateScales'
        tickIdentifier
        (Settings baseTolerance offsetter)
        generator


generateScales' :: (InternalFloat -> [(InternalFloat, ScaleID)]) -> Settings -> Generator a -> M.Map ScaleID (S.Seq Tick)
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

generateTicksOnly :: Settings -> Generator a -> [Tick]
generateTicksOnly settings = Set.toList . fst . unlogging . generate settings

-- TICK IDENTIFIERS

defaultIdentifier :: InternalFloat -> [(InternalFloat, ScaleID)]
defaultIdentifier x = [(x, SID "default")]

floorIdentifier :: InternalFloat -> (Integer, Integer) -> InternalFloat -> [(InternalFloat, ScaleID)]
floorIdentifier leeway (lower, upper) x =
    let iids = nub $ map floor [x - leeway, x, x + leeway]
    in
    [ (x - fromIntegral iid, IID iid)
    | iid <- iids
    , iid >= lower
    , iid < upper
    ]

-- RENDERING

renderScales :: (Renderer a, Monoid (Representation a)) => Proxy a -> ScaleSpec -> Representation a
renderScales proxya spec@ScaleSpec{ renderSettings } =
    (foldMap . foldMap) (renderTick proxya renderSettings) (generateScales spec)

writeScalesToFile :: (Renderer a, Monoid (Representation a)) => Proxy a -> FilePath -> ScaleSpec -> IO ()
writeScalesToFile proxya filepath spec =
    writeRepToFile proxya filepath (renderScales proxya spec)

