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
    let part9  = Partition 9 0 $ fromInfoX $ \info x -> info & end .~ 1 <<< mlabel . mayDef %~ (labelCenterOver 0.002 <<< fontSize .~ 0.6 <<< text .~ showF round x)
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
          partitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [ (1.002 , 5)
            , (1.0025, 5)
            , (1.003 , 1)
            , (1.004 , 1)
            , (1.005 , 1)
            , (1.006 , 1)
            , (1.007 , 1)
            , (1.008 , 1)
            , (1.009 , 1)
            , (1.010 , 5)
            , (1.015 , 5)
            , (1.02  , 1)
            , (1.03  , 1)
            ]

ll2 :: Generator ()
ll2
  = let part2  h = Partition 2 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
        part5  h = Partition 5 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
        part10 h = Partition 10 0 $ fromInfo (end %~ (h*) <<< mlabel .~ Nothing)
        subtrees h =
            [ OptionTree [part2 $ h * 0.7, part5 $ h * 0.5] [(0, 9, subtrees $ h * 0.5)]
            , OptionTree [part5 $ h * 0.5] []
            , OptionTree [part2 $ h * 0.5] []
            ]
    in
    postPostTransform (Within 0 1) $
      postTransform (Offset 2) $
        postTransform (LogLog 10) $
          partitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [ (1.02 , 5)
            , (1.025, 5)
            , (1.03 , 1)
            , (1.04 , 1)
            , (1.05 , 1)
            , (1.06 , 1)
            , (1.07 , 1)
            , (1.08 , 1)
            , (1.09 , 1)
            , (1.10 , 1)
            , (1.15 , 5)
            , (1.2  , 5)
            , (1.25 , 5)
            , (1.3  , 1)
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
          partitionTens 0.002 (\n -> [(0,n-1)]) (subtrees 1)
            [ ( 1.25, 5)
            , ( 1.30, 5)
            , ( 1.35, 5)
            , ( 1.4 , 1)
            , ( 1.5 , 1)
            , ( 1.6 , 1)
            , ( 1.7 , 1)
            , ( 1.8 , 1)
            , ( 1.9 , 1)
            , ( 2.0 , 5)
            , ( 2.5 , 5)
            , ( 3   , 1)
            , ( 4   , 1)
            , ( 5   , 1)
            , ( 6   , 1)
            , ( 7   , 1)
            , ( 8   , 1)
            , ( 9   , 1)
            , (10   , 1)
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
          partitionTens 0.002 (\n -> if n == 9 then [(0,3),(4,8)] else [(0,n-1)]) (subtrees 1)
            [ (  10, 5)
            , (  15, 5)
            , (  20, 1)
            , (  30, 1)
            , (  40, 1)
            , (  50, 1)
            , ( 1e2, 1)
            , ( 2e2, 3)
            , ( 5e2, 5)
            , ( 1e3, 1)
            , ( 2e3, 3)
            , ( 5e3, 5)
            , ( 1e4, 1)
            , ( 2e4, 3)
            , ( 5e4, 5)
            , ( 1e5, 9)
            , ( 1e6, 9)
            , ( 1e7, 9)
            , ( 1e8, 9)
            , ( 1e9, 9)
            , (1e10, 9)
            ]

renderSlide :: Generator a -> D.Diagram D.B
renderSlide generator =
    let ticks = foldMap (renderTick False) $ _out $ generate generator
    in
    ticks <> laserline [D.r2 (0, 0), D.r2 (1, 0)]

total :: D.Diagram D.B
total = D.bgFrame 0.025 D.white $ D.vsep 0.02 $ map renderSlide [ c, ll1, ll2, ll3, ll4 ]
