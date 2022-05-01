{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    { _start  :: InternalFloat
    , _end    :: InternalFloat
    , _mlabel :: Maybe Label
    , _tickColor :: D.AlphaColour Double
    }
    deriving (Show, Generic)

instance NFData TickInfo
instance NFData c => NFData (D.AlphaColour c) where
  rnf _c = ()

instance Default TickInfo where
    def =
        TickInfo
            { _start = 0
            , _end = 1
            , _mlabel = Nothing
            , _tickColor = D.opaque D.red
            }

data Label = Label
    { _fontSize     :: InternalFloat
    , _text         :: String
    , _textAnchor   :: TextAnchor
    , _tickAnchor   :: TickAnchor
    , _anchorOffset :: D.V2 InternalFloat
    , _labelColor   :: D.AlphaColour Double
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
            , _labelColor = D.opaque D.black
            }

data TextAnchor = TextAnchor
    { _xPct :: InternalFloat
    , _yPct :: InternalFloat
    }
    deriving (Show, Generic)

instance NFData TextAnchor

data TickAnchor = Pct InternalFloat | FromTopAbs InternalFloat | FromBottomAbs InternalFloat
    deriving (Show, Generic)

instance NFData TickAnchor

makeLenses ''TickF
makeLenses ''TickInfo
makeLenses ''Label
makeLenses ''TickAnchor
makeLenses ''TextAnchor

-- COMMON ANCHORINGS

labelCenterOver :: InternalFloat -> Label -> Label
labelCenterOver margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 margin
    }

labelCenterUnder :: InternalFloat -> Label -> Label
labelCenterUnder margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 $ negate margin
    }

labelRight :: InternalFloat -> Label -> Label
labelRight margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 margin 0
    }

labelRightCenter :: InternalFloat -> Label -> Label
labelRightCenter margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0.5 }
    , _tickAnchor = Pct 0.5
    , _anchorOffset = D.V2 margin 0
    }

labelRightAbove :: InternalFloat -> InternalFloat -> Label -> Label
labelRightAbove marginX marginY label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 marginX marginY
    }

-- Label lens

label :: Lens' TickInfo Label
label = mlabel . mayDefL
