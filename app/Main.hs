module Main where

-- base
import Data.Foldable
import System.Environment

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
main = SlideRules.example
