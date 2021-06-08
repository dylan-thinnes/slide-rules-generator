{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module SlideRules.Tick where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Numeric

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- lens
import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)

-- local (sliderules)
import SlideRules.Types
import SlideRules.Utils

data TickG info = Tick
    { _prePos      :: InternalFloat
    , _postPos     :: InternalFloat
    , _postPostPos :: Maybe InternalFloat
    , _info        :: info
    }
    deriving Show

type Tick = TickG TickInfo

instance Eq (TickG a) where
    a == b = _postPos a == _postPos b
instance Ord (TickG a) where
    compare a b = compare (_postPos a) (_postPos b)

instance Default info => Default (TickG info) where
    def =
        Tick
            { _prePos = 0
            , _postPos = 0
            , _postPostPos = Just 0
            , _info = def
            }

data TickInfo = TickInfo
    { _start  :: Double
    , _end    :: Double
    , _mlabel :: Maybe Label
    }
    deriving (Show)

instance Default TickInfo where
    def =
        TickInfo
            { _start = 0
            , _end = 1
            , _mlabel = Nothing
            }

data Label = Label
    { _fontSize     :: Double
    , _text         :: String
    , _textAnchor   :: TextAnchor
    , _tickAnchor   :: TickAnchor
    , _anchorOffset :: D.V2 Double
    }
    deriving Show

instance Default Label where
    def =
        Label
            { _fontSize = 0
            , _text = ""
            , _textAnchor = TextAnchor { _xPct = 0, _yPct = 0 }
            , _tickAnchor = FromTopAbs 0
            , _anchorOffset = D.V2 0 0
            }

data TextAnchor = TextAnchor
    { _xPct :: Double
    , _yPct :: Double
    }
    deriving Show

data TickAnchor = Pct Double | FromTopAbs Double | FromBottomAbs Double
    deriving Show

makeLenses ''TickG
makeLenses ''TickInfo
makeLenses ''Label
makeLenses ''TickAnchor
makeLenses ''TextAnchor

hScale = 0.02

renderTick :: Tick -> D.Diagram D.B
renderTick tick =
    let Tick { _prePos, _postPos, _postPostPos, _info } = tick
        TickInfo { _start, _end, _mlabel } = _info
        startV2 = D.r2 (0, hScale * _start)
        endV2   = D.r2 (0, hScale * _end)
        diffV2  = endV2 - startV2
        tickDia = laserline [diffV2] & D.translate startV2
        labelDia = fromMaybe mempty $ do
            Label {..} <- _mlabel
            let labelOffset :: D.V2 Double
                labelOffset
                  = _anchorOffset * D.r2 (1, hScale)
                  + case _tickAnchor of
                      Pct p -> startV2 + diffV2 * D.V2 p p
                      FromTopAbs x -> endV2 + D.r2 (0, hScale * x)
                      FromBottomAbs x -> startV2 + D.r2 (0, hScale * x)
            pure $
                D.alignedText (_xPct _textAnchor) (_yPct _textAnchor) _text
                  & D.fontSizeL (hScale * _fontSize) & D.fc D.black
                  & D.font "Comfortaa"
                  & D.translate labelOffset
     in case _postPostPos of
         Nothing -> mempty
         Just ppp -> D.lc D.red tickDia <> labelDia & D.translate (D.r2 (realToFrac ppp, 0))

-- COMMON ANCHORINGS

labelCenterOver :: Double -> Label -> Label
labelCenterOver margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 margin
    }

labelCenterUnder :: Double -> Label -> Label
labelCenterUnder margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 $ negate margin
    }

labelRight :: Double -> Label -> Label
labelRight margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 margin 0
    }

-- Label lens

label :: Lens' TickInfo Label
label = mlabel . mayDefL
