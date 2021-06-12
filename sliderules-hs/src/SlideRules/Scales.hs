{-# LANGUAGE TupleSections #-}
module SlideRules.Scales where

-- base
import Data.Foldable (fold)
import Data.List (nub)

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

data ScaleID = IID Integer | SID String
    deriving (Eq, Ord)

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

renderScaleTicks :: Foldable f => f Tick -> D.Diagram D.B
renderScaleTicks ticks =
    let tickDias = foldMap renderTick ticks
        underlineDia = D.lc D.blue (laserline [D.r2 (0, 0), D.r2 (1, 0)])
        anchorDia = D.lc D.green (laserline [D.r2 (0, 0), D.r2 (-0.01, 0), D.r2 (0, 0.01)])
    in
    tickDias <> underlineDia <> anchorDia

renderScaleTicksCircular :: Foldable f => InternalFloat -> f Tick -> D.Diagram D.B
renderScaleTicksCircular radius ticks =
    let tickDias = foldMap (renderTickCircular radius) ticks
        -- underlineDia = D.lc D.blue $ D.lineWidth D.ultraThin $ D.circle radius
        anchorDia = D.lc D.green (laserline [D.r2 (0, 0), D.r2 (-0.01, 0), D.r2 (0, 0.01)])
    in
    tickDias <> anchorDia

genAndRender :: (InternalFloat -> [(InternalFloat, ScaleID)]) -> Settings -> Generator a -> [D.Diagram D.B]
genAndRender tickIdentifiers settings =
    fmap renderScaleTicks . M.elems . generateScales tickIdentifiers settings

genAndRenderSingle :: Settings -> Generator a -> [D.Diagram D.B]
genAndRenderSingle = genAndRender defaultIdentifier

defaultIdentifier :: InternalFloat -> [(InternalFloat, ScaleID)]
defaultIdentifier x = [(x, SID "default")]

genAndRenderFloor :: InternalFloat -> (Integer, Integer) -> Settings -> Generator a -> [D.Diagram D.B]
genAndRenderFloor leeway (lower, upper) = genAndRender (floorIdentifier leeway (lower, upper))

floorIdentifier :: InternalFloat -> (Integer, Integer) -> InternalFloat -> [(InternalFloat, ScaleID)]
floorIdentifier leeway (lower, upper) x =
    let iids = nub $ map floor [x - leeway, x, x + leeway]
    in
    [ (x - fromIntegral iid, IID iid)
    | iid <- iids
    , iid >= lower
    , iid < upper
    ]
