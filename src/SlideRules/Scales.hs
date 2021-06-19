{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module SlideRules.Scales where

-- base
import Data.Foldable (fold)
import Data.List (nub)
import Data.Maybe (isJust)

-- containers
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

-- SCALE SPECS

data ScaleID = IID Integer | SID String
    deriving (Eq, Ord, Show)

data ScaleSpec = ScaleSpec
    { heightMultiplier :: InternalFloat
    , textMultiplier :: InternalFloat
    , baseTolerance :: InternalFloat
    , tickIdentifier :: InternalFloat -> [(InternalFloat, ScaleID)]
    , generator :: Generator ()
    , offsetter :: Offsetter
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

genRenderScaleSpec :: ScaleSpec -> [D.Diagram D.B]
genRenderScaleSpec spec@ScaleSpec {..}
  = let identifiedTicks = generateScales spec
        anchorDia = D.lc D.green (laserline [D.r2 (0, 0), D.r2 (-0.01, 0), D.r2 (0, 0.01)])
    in
    flip map (M.toList identifiedTicks) $ \(scaleID, ticks) ->
        anchorDia <>
        foldMap (renderTick heightMultiplier textMultiplier) ticks

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
