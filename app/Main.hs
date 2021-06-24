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
import qualified SlideRules

main :: IO ()
main = do
    writeToFasterSVG "out-faster.svg" SlideRules.cSpecLong
    writeToFastSVG "out-fast.svg" SlideRules.cSpecLong
    writeToFile "out.svg" (fold $ genRenderScaleSpec SlideRules.cSpecLong)
