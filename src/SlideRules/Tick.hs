{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module SlideRules.Tick where

-- base
import Data.Function ((&))
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Numeric
import GHC.Generics

-- deepseq
import Control.DeepSeq

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

data OffsetF a = Radial a | Vertical a
    deriving (Show, Functor, Generic)

instance NFData a => NFData (OffsetF a)

type Offset = OffsetF InternalFloat
type Offsetter = OffsetF (InternalFloat -> InternalFloat)

applyOffsetter :: Offsetter -> InternalFloat -> Offset
applyOffsetter offsetter x = fmap ($ x) offsetter

data TickF info = Tick
    { _prePos      :: InternalFloat
    , _postPos     :: InternalFloat
    , _offset      :: Offset
    , _info        :: info
    }
    deriving (Show, Functor, Generic)

instance NFData info => NFData (TickF info)

type Tick = TickF TickInfo

truePos :: TickF a -> InternalFloat
truePos Tick { _postPos, _offset } =
    case _offset of
        Vertical _ -> _postPos
        Radial r -> _postPos * 2 * pi * r

instance Eq (TickF a) where
    a == b = _postPos a == _postPos b
instance Ord (TickF a) where
    compare a b = compare (_postPos a) (_postPos b)

deinfo :: TickF info -> TickF ()
deinfo = fmap (const ())

instance Default info => Default (TickF info) where
    def =
        Tick
            { _prePos = 0
            , _postPos = 0
            , _offset = Vertical 0
            , _info = def
            }

data TickInfo = TickInfo
    { _start  :: Double
    , _end    :: Double
    , _mlabel :: Maybe Label
    }
    deriving (Show, Generic)

instance NFData TickInfo

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
    deriving (Show, Generic)

instance NFData Label

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
    deriving (Show, Generic)

instance NFData TextAnchor

data TickAnchor = Pct Double | FromTopAbs Double | FromBottomAbs Double
    deriving (Show, Generic)

instance NFData TickAnchor

makeLenses ''TickF
makeLenses ''TickInfo
makeLenses ''Label
makeLenses ''TickAnchor
makeLenses ''TextAnchor

renderTick :: InternalFloat -> InternalFloat -> Tick -> D.Diagram D.B
renderTick heightMultiplier textMultiplier tick =
    let staticTick = renderTickStatic heightMultiplier textMultiplier tick
    in
    case _offset tick of
        Vertical y ->
            staticTick
                & D.translate (D.r2 (realToFrac $ _postPos tick, y))
        Radial rad ->
            staticTick
                & D.translate (D.r2 (0, rad))
                & D.rotateBy (negate $ realToFrac $ _postPos tick)

renderTickStatic :: InternalFloat -> InternalFloat -> Tick -> D.Diagram D.B
renderTickStatic heightMultiplier textMultiplier tick =
    let Tick { _prePos, _postPos, _info } = tick
        TickInfo { _start, _end, _mlabel } = _info
        startV2 = D.r2 (0, heightMultiplier * _start)
        endV2   = D.r2 (0, heightMultiplier * _end)
        diffV2  = endV2 - startV2
        tickDia = laserline [diffV2] & D.translate startV2
        labelDia = fromMaybe mempty $ do
            Label {..} <- _mlabel
            let labelOffset :: D.V2 Double
                labelOffset
                  = _anchorOffset * D.r2 (1, heightMultiplier)
                  + case _tickAnchor of
                      Pct p -> startV2 + diffV2 * D.V2 p p
                      FromTopAbs x -> endV2 + D.r2 (0, heightMultiplier * x)
                      FromBottomAbs x -> startV2 + D.r2 (0, heightMultiplier * x)
            pure $
                D.alignedText (_xPct _textAnchor) (_yPct _textAnchor) _text
                  & D.fontSizeL (heightMultiplier * textMultiplier * _fontSize) & D.fc D.black
                  & D.font "Comfortaa"
                  & D.translate labelOffset
     in D.lc D.red tickDia <> labelDia

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

labelRightCenter :: Double -> Label -> Label
labelRightCenter margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0.5 }
    , _tickAnchor = Pct 0.5
    , _anchorOffset = D.V2 margin 0
    }

-- Label lens

label :: Lens' TickInfo Label
label = mlabel . mayDefL
