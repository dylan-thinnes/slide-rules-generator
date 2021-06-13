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
    deriving (Eq, Ord)

data ScaleSpec = ScaleSpec
    { heightMultiplier :: InternalFloat
    , baseTolerance :: InternalFloat
    , tickIdentifier :: InternalFloat -> [(InternalFloat, ScaleID)]
    , generator :: Generator ()
    , circular :: Maybe (InternalFloat -> InternalFloat)
    }

-- GENERATE SCALES

generateScales :: (InternalFloat -> [(InternalFloat, ScaleID)]) -> Settings -> Generator a -> M.Map ScaleID (S.Seq Tick)
generateScales tickIdentifiers settings generator =
    let ticks = generateTicksOnly settings generator
        tickIdentifiersFromPostPost = maybe [] tickIdentifiers . _postPostPos
        insertTick tick map =
            foldr
                (\(x', id) -> M.insertWith (<>) id (S.singleton $ tick { _postPostPos = Just x' }))
                map
                (tickIdentifiersFromPostPost tick)
        identifiedTicks = foldr insertTick M.empty ticks
    in
    identifiedTicks

generateTicksOnly :: Settings -> Generator a -> S.Seq Tick
generateTicksOnly settings = _out . generate settings

genRenderScaleSpec :: ScaleSpec -> [D.Diagram D.B]
genRenderScaleSpec ScaleSpec {..}
  = let identifiedTicks =
            generateScales
                tickIdentifier
                (Settings baseTolerance circular)
                generator

        anchorDia = D.lc D.green (laserline [D.r2 (0, 0), D.r2 (-0.01, 0), D.r2 (0, 0.01)])
    in
    flip map (M.toList identifiedTicks) $ \(scaleID, ticks) ->
        fold $
            if isJust circular
              then
                [ foldMap (renderTickCircular heightMultiplier) ticks
                , mempty
                , anchorDia
                ]
              else
                [ foldMap (renderTickLinear heightMultiplier) ticks
                , D.lc D.blue (laserline [D.r2 (0, 0), D.r2 (1, 0)])
                , anchorDia
                ]

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
