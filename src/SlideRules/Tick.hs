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

type Offset fl = OffsetF fl
type Offsetter fl = OffsetF (fl -> fl)

applyOffsetter :: Offsetter fl -> fl -> Offset fl
applyOffsetter offsetter x = fmap ($ x) offsetter

data TickF fl info = Tick
    { _prePos      :: fl
    , _postPos     :: fl
    , _offset      :: Offset fl
    , _info        :: info
    }
    deriving (Show, Functor, Generic)

instance (NFData fl, NFData info) => NFData (TickF fl info)

type Tick fl = TickF fl (TickInfo fl)

truePos :: (Floating fl, Num fl) => TickF fl info -> fl
truePos Tick { _postPos, _offset } =
    case _offset of
        Vertical _ -> _postPos
        Radial r -> _postPos * 2 * pi * r

instance Eq fl => Eq (TickF fl a) where
    a == b = _postPos a == _postPos b
instance Ord fl => Ord (TickF fl a) where
    compare a b = compare (_postPos a) (_postPos b)

deinfo :: TickF fl info -> TickF fl ()
deinfo = fmap (const ())

instance (Num fl, Default info) => Default (TickF fl info) where
    def =
        Tick
            { _prePos = 0
            , _postPos = 0
            , _offset = Vertical 0
            , _info = def
            }

data TickInfo fl = TickInfo
    { _start  :: fl
    , _end    :: fl
    , _mlabel :: Maybe (Label fl)
    }
    deriving (Show, Generic)

instance NFData fl => NFData (TickInfo fl)

instance (Num fl, Default fl) => Default (TickInfo fl) where
    def =
        TickInfo
            { _start = 0
            , _end = 1
            , _mlabel = Nothing
            }

data Label fl = Label
    { _fontSize     :: fl
    , _text         :: String
    , _textAnchor   :: TextAnchor fl
    , _tickAnchor   :: TickAnchor fl
    , _anchorOffset :: D.V2 fl
    }
    deriving (Show, Generic)

instance NFData fl => NFData (Label fl)

instance (Num fl, Default fl) => Default (Label fl) where
    def =
        Label
            { _fontSize = 0
            , _text = ""
            , _textAnchor = TextAnchor { _xPct = 0, _yPct = 0 }
            , _tickAnchor = FromTopAbs 0
            , _anchorOffset = D.V2 0 0
            }

data TextAnchor fl = TextAnchor
    { _xPct :: fl
    , _yPct :: fl
    }
    deriving (Show, Generic)

instance NFData fl => NFData (TextAnchor fl)

data TickAnchor fl = Pct fl | FromTopAbs fl | FromBottomAbs fl
    deriving (Show, Generic)

instance NFData fl => NFData (TickAnchor fl)

makeLenses ''TickF
makeLenses ''TickInfo
makeLenses ''Label
makeLenses ''TickAnchor
makeLenses ''TextAnchor

-- COMMON ANCHORINGS

labelCenterOver :: (Fractional fl, Num fl) => fl -> Label fl -> Label fl
labelCenterOver margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 0 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 margin
    }

labelCenterUnder :: (Fractional fl, Num fl) => fl -> Label fl -> Label fl
labelCenterUnder margin label = label
    { _textAnchor = TextAnchor { _xPct = 0.5, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 0 $ negate margin
    }

labelRight :: Num fl => fl -> Label fl -> Label fl
labelRight margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 1 }
    , _tickAnchor = FromTopAbs 0
    , _anchorOffset = D.V2 margin 0
    }

labelRightCenter :: (Fractional fl, Num fl) => fl -> Label fl -> Label fl
labelRightCenter margin label = label
    { _textAnchor = TextAnchor { _xPct = 0, _yPct = 0.5 }
    , _tickAnchor = Pct 0.5
    , _anchorOffset = D.V2 margin 0
    }

-- Label lens

label :: (Num fl, Default fl) => Lens' (TickInfo fl) (Label fl)
label = mlabel . mayDefL
