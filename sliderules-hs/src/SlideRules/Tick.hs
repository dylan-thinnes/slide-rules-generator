{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module SlideRules.Tick where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)

-- default
import Data.Default

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- local (sliderules)
import SlideRules.Types
import SlideRules.Utils

data Tick = Tick
    { prePos  :: InternalFloat
    , postPos :: InternalFloat
    , start   :: Double
    , end     :: Double
    , mlabel   :: Maybe Label
    }
    deriving Show

data Label = Label
    { fontSize     :: Double
    , text         :: String
    , textAnchor   :: TextAnchor
    , tickAnchor   :: TickAnchor
    , anchorOffset :: D.V2 Double
    }
    deriving Show

instance Default Label where
    def =
        Label
            { fontSize = 0
            , text = ""
            , textAnchor = TextAnchor { xPct = 0, yPct = 0 }
            , tickAnchor = FromTopAbs 0
            , anchorOffset = D.V2 0 0
            }

data TextAnchor = TextAnchor
    { xPct :: Double
    , yPct :: Double
    }
    deriving Show

data TickAnchor = Pct Double | FromTopAbs Double | FromBottomAbs Double
    deriving Show

hScale = 0.02

renderTick :: Bool -> Tick -> D.Diagram D.B
renderTick above Tick { prePos, postPos, start, end, mlabel } =
    let startV2 = D.r2 (0, hScale * start)
        endV2   = D.r2 (0, hScale * end)
        diffV2  = endV2 - startV2
        tickDia = laserline [diffV2] & D.translate startV2
        labelDia = fromMaybe mempty $ do
            Label {..} <- mlabel
            let labelOffset :: D.V2 Double
                labelOffset
                  = anchorOffset * D.r2 (1, hScale)
                  + case tickAnchor of
                      Pct p -> startV2 + diffV2 * D.V2 p p
                      FromTopAbs x -> endV2 + D.r2 (0, hScale * x)
                      FromBottomAbs x -> startV2 + D.r2 (0, hScale * x)
            pure $
                D.alignedText (xPct textAnchor) (yPct textAnchor) text
                  & D.fontSizeL (hScale * fontSize) & D.fc D.black
                  & D.font "Comfortaa"
                  & D.translate labelOffset
     in tickDia <> labelDia & D.translate (D.r2 (realToFrac postPos, 0))
