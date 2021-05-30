{-# LANGUAGE TemplateHaskell #-}
module SlideRules.Lenses where

-- lens
import Control.Lens.TH (makeLenses)

-- local (sliderules)
import SlideRules.Tick

makeLenses ''Tick
makeLenses ''TickInfo
makeLenses ''TickAnchor
makeLenses ''TextAnchor
