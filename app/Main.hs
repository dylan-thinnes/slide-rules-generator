module Main where

-- base
import Data.Foldable

-- decimal
import Data.Decimal

-- lens
import Control.Lens

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils
import SlideRules.Scales
import SlideRules.IO

main :: IO ()
main = writeToFasterSVG "out.svg" cSpecLong
--main = writeToFastSVG "out.svg" cSpecLong
--main = writeToFile "out.svg" total

total =
    (fold . fold) $
        [ genRenderScaleSpec cSpecLong
        ]
    -- foldMap (fold . genRenderScaleSpec)
    --   [ cSpec
    --   , cSpec { offsetter = unitRadius 1 }
    --   , cSpec { offsetter = unitRadius 2 }
    --   , aSpec { offsetter = unitRadius 1.5 }
    --   , llSpec
    --   ]

cSpecLong :: ScaleSpec
cSpecLong = ScaleSpec
    { heightMultiplier = 0.02
    , textMultiplier = 1
    , baseTolerance = 0.03
    , tickIdentifier = floorIdentifier 0.00001 (0, 100)
    , generator =
        withs
            [ postTransform (Scale 100)
            , postTransform (Log 10)
            , withXInfo $ \x -> label %~ (labelRightCenter 0.002 <<< fontSize .~ 0.5 <<< text .~ (show $ decimalMantissa $ normalizeDecimal $ roundTo 8 $ realToFrac x))
            , preTransform (Offset 1)
            , preTransform (Scale 9)
            ] $
            let tree2 = OptionTree [Partition 2 0 id] []
                tree5 = OptionTree [Partition 5 0 id] []
                tree10 = fillOptionTree [Partition 10 0 id] [tree10, tree5, tree2]
                optionTree = fillOptionTree [Partition 9 0 id] [tree10, tree5, tree2]
            in
            runOptionTrees (True, True) [optionTree]
    , offsetter = incline 0.02
    }
