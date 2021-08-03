module SlideRules.SerializableGenerator where

-- aeson
import Data.Aeson
import qualified Data.Aeson.Parser as Parser

-- base
import GHC.Generics

-- decimal
import Data.Decimal

-- lens
import Control.Lens (use, (%~), (.~))

-- scientific
import Data.Scientific

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Tick
import SlideRules.Utils
