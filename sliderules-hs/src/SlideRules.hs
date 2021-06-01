module SlideRules where

-- base
import Data.Function ((&))

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- lens
import Control.Lens

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Lenses
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

ex100 :: Generator ()
ex100 =
    postTransform (Log 10) $
    withInfoX (\info x -> info & mlabel . mayDef %~ (fontSize .~ 0.6 <<< text .~ show x <<< textAnchor .~ TextAnchor { _xPct = 0.5, _yPct = 0 })) $
    together
        [ do
            x <- list [1..9]
            output x
            withInfo (end .~ 0.5 <<< mlabel .~ Nothing) $
                preTransform (Offset x) $ preTransform (Scale 0.1) $ do
                    x <- list [1..9]
                    output x
        , output 10
        , withInfo (start .~ 0.6 <<< end .~ 0.7) $
            together
                [ withInfo (mlabel . mayDef . text .~ "π") $ output pi
                , withInfo (mlabel . mayDef . text .~ "e") $ output e
                ]
        ]

ex200 :: Generator ()
ex200 = postTransform (Log 10) $  preTransform (Offset 1) $ preTransform (Scale 10) $
    bestPartitions 0.0001 $
        let part2 = Partition 2 0 $ fromInfo (end %~ (* 0.75))
            part5 = Partition 5 0 $ fromInfo (end %~ (* 0.66))
            part9 = Partition 9 0 $ fromInfo (end .~ 1)
            tree = OptionTree [part9] [(0, 9, subtrees 1)]
            subtrees depth =
                [ OptionTree [part2, part5] $ if depth == 0 then [] else [(0, 10, subtrees $ depth - 1)]
                , OptionTree [part5] []
                , OptionTree [part2] []
                ]
        in
        tree

--ex200 = postTransform (Log 10) $ preTransform (Scale 10) $
--    bestPartitions 0.002 $
--        let part2 = Partition 2 0 $ fromInfo (end %~ (* 0.75))
--            part5 = Partition 5 0 $ fromInfo (end %~ (* 0.66))
--            part9 = Partition 9 1 $ fromInfo (end .~ 1)
--            tree = OptionTree [part9] subtrees
--            subtrees =
--                [ OptionTree [part2, part5] subtrees
--                , OptionTree [part5] []
--                , OptionTree [part2] []
--                ]
--        in
--        [tree]

renderSlide :: Generator a -> D.Diagram D.B
renderSlide generator =
    let ticks = foldMap (renderTick False) $ _out $ generate generator
    in
    ticks <> laserline [D.r2 (0, 0), D.r2 (1, 0)]

total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 [ renderSlide ex200 ]
