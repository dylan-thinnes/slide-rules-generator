module Main where

import Data.Foldable
import Control.Lens
import Data.Decimal

import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils
import SlideRules.Scales
import SlideRules.IO

main :: IO ()
main = writeToFile "out.svg" $ fold $ genRenderScaleSpec cSpecLong

cSpecLong :: ScaleSpec
cSpecLong = ScaleSpec
    { heightMultiplier = 0.02
    , textMultiplier = 1
    , baseTolerance = 0.04
    , tickIdentifier = floorIdentifier 0 (0, 100)
    , generator =
        let part5 = Partition 5 0 id
            part9  = Partition 9 0 id
            part10 = Partition 10 0 id
            tree5 = OptionTree [part5] []
            tree10 = fillOptionTree [part10] [tree10, tree5]
            optionTree = fillOptionTree [part9] [tree10, tree5]
        in
        withs
            [ postTransform (Scale 100)
            , postTransform (Log 10)
            , withXInfo $ \x -> label %~ (labelRight 0.002 <<< fontSize .~ 0.5 <<< text .~ (show $ normalizeDecimal $ roundTo 8 $ realToFrac x))
            , preTransform (Offset 1)
            , preTransform (Scale 9)
            ]
            (runOptionTrees (True, True) [optionTree])
    , offsetter = incline 0.03
    }

