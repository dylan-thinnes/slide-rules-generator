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

c :: Generator ()
c = postTransform (Log 10) $  preTransform (Offset 1) $ preTransform (Scale 9) $
    let part9  = Partition 9 0 $ fromInfoX $ \info x -> info & end .~ 1 <<< mlabel . mayDef %~ (fontSize .~ 0.6 <<< text .~ show x)
        part2  = Partition 2 0 $ fromInfo (end %~ (* 0.75) <<< mlabel .~ Nothing)
        part5  = Partition 5 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
        part10 = Partition 10 0 $ fromInfo (end %~ (* 0.66) <<< mlabel .~ Nothing)
        tree = fillOptionTree [part9] subtrees
        subtrees =
            [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
            , OptionTree [part5] []
            , OptionTree [part2] []
            ]
     in do
        mPartitionTree <- bestPartitions 0.002 tree
        saveToLog $ show mPartitionTree
        maybeM () runPartitionTree mPartitionTree

ll1 :: Generator ()
ll1
  = let part2  h = Partition 2 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part5  h = Partition 5 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part10 h = Partition 10 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        subtrees h =
            [ OptionTree [part2 $ h * 0.7, part5 $ h * 0.5] [(0, 9, subtrees $ h * 0.5)]
            , OptionTree [part5 $ h * 0.5] []
            , OptionTree [part2 $ h * 0.5] []
            ]
    in
    postPostTransform (Within 0 1) $
      postTransform (Offset 3) $
        postTransform (LogLog 10) $
          smartPartitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [ 1.002
            , 1.0025
            , 1.003
            , 1.004
            , 1.005
            , 1.006
            , 1.007
            , 1.008
            , 1.009
            , 1.010
            , 1.015
            , 1.02
            , 1.03
            ]

ll2 :: Generator ()
ll2
  = let part2  h = Partition 2 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part5  h = Partition 5 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part10 h = Partition 10 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        subtrees h =
            [ OptionTree [part2 $ h * 0.7, part5 $ h * 0.5] [(0, 9, subtrees $ h * 0.5)]
            , OptionTree [part5 $ h * 0.5] []
            , OptionTree [part2 $ h * 0.5] []
            ]
    in
    postPostTransform (Within 0 1) $
      postTransform (Offset 2) $
        postTransform (LogLog 10) $
          smartPartitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [ 1.02
            , 1.03
            , 1.04
            , 1.05
            , 1.06
            , 1.07
            , 1.08
            , 1.09
            , 1.10
            , 1.15
            , 1.2
            , 1.25
            , 1.3
            ]

ll3 :: Generator ()
ll3
  = let part2  h = Partition 2 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part5  h = Partition 5 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part10 h = Partition 10 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        subtrees h =
            [ OptionTree [part2 $ h * 0.7, part5 $ h * 0.5] [(0, 9, subtrees $ h * 0.5)]
            , OptionTree [part5 $ h * 0.5] []
            , OptionTree [part2 $ h * 0.5] []
            ]
    in
    postPostTransform (Within 0 1) $
      postTransform (Offset 1) $
        postTransform (LogLog 10) $
          smartPartitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [  1.25
            ,  1.30
            ,  1.35
            ,  1.4
            ,  1.5
            ,  1.6
            ,  1.7
            ,  1.8
            ,  1.9
            ,  2.0
            ,  2.5
            ,  3
            ,  4
            ,  5
            ,  6
            ,  7
            ,  8
            ,  9
            , 10
            ]

ll4 :: Generator ()
ll4
  = let part2  h = Partition 2 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part5  h = Partition 5 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        part10 h = Partition 10 0 $ fromInfo (end .~ h <<< mlabel .~ Nothing)
        subtrees h =
            [ OptionTree [part2 $ h * 0.7, part5 $ h * 0.5] [(0, 9, subtrees $ h * 0.5)]
            , OptionTree [part5 $ h * 0.5] []
            , OptionTree [part2 $ h * 0.5] []
            ]
    in
    postPostTransform (Within 0 1) $
      postTransform (Offset 0) $
        postTransform (LogLog 10) $
          smartPartitionTens 0.002 (\n -> if n == 9 then [(0,3),(4,8)] else [(0,n-1)]) (subtrees 1)
            [ 10
            , 15
            , 20
            , 30
            , 40
            , 50
            , 1e2
            , 2e2
            , 5e2
            , 1e3
            , 2e3
            , 5e3
            , 1e4
            , 2e4
            , 5e4
            , 1e5
            , 1e6
            , 1e7
            , 1e8
            , 1e9
            , 1e10
            ]

renderSlide :: Generator a -> D.Diagram D.B
renderSlide generator =
    let ticks = foldMap (renderTick False) $ _out $ generate generator
    in
    ticks <> laserline [D.r2 (0, 0), D.r2 (1, 0)]

total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 $ map renderSlide [ c, ll1, ll2, ll3, ll4 ]
