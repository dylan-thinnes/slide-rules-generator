module Main where

import           Control.Monad.State
import           Data.List
import           Numeric.Decimal

import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

main = pure ()

type InternalFloat = BasicDecimal

type Gen meta = StateT (GenState meta) []

-- TICKS AND THEIR METADATA
data Tick = Tick
    { prePos  :: InternalFloat
    , postPos :: InternalFloat
    , meta    :: Meta
    }
    deriving (Show)

data GenState meta = GenState
    { _preTrans  :: () -- Transform
    , _postTrans :: () -- Transform
    , _out       :: () -- S.Seq (Tick meta)
    }
    deriving (Show)

data Meta = Meta { _start :: Double, _end :: Double, _mtext :: Maybe TextMeta }
    deriving Show

data TextAnchor = TextAnchor
    { textAnchorXPct :: Double
    , textAnchorYPct :: Double
    }
    deriving Show

data TickAnchor = Pct Double | Abs Double
    deriving Show

data Anchor = Anchor
    { textAnchor   :: TextAnchor
    , tickAnchor   :: TickAnchor
    , anchorOffset :: D.V2 Double
    }
    deriving Show

alongsideTopLeft, aboveCenter :: Double -> Anchor
alongsideTopLeft sep =
    Anchor
        { textAnchor = TextAnchor
            { textAnchorXPct = 0
            , textAnchorYPct = 1
            }
        , tickAnchor = Pct 1
        , anchorOffset = D.V2 sep 0
        }
aboveCenter sep =
    Anchor
        { textAnchor = TextAnchor
            { textAnchorXPct = 0.5
            , textAnchorYPct = 0
            }
        , tickAnchor = Pct 1
        , anchorOffset = D.V2 0 sep
        }

data TextMeta = TextMeta
    { _anchor   :: Anchor
    , _fontSize :: Double
    , _text     :: String
    }
    deriving Show
