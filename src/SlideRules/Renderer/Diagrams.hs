{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module SlideRules.Renderer.Diagrams where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)

-- bytestring
import qualified Data.ByteString.Lazy

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- svg-builder
import qualified Graphics.Svg.Core

-- text
import qualified Data.Text                    as T

-- local (sliderules)
import SlideRules.Renderer
import SlideRules.Tick
import SlideRules.Types
import SlideRules.Utils

data Dias

instance Renderer Dias where
    type Representation Dias = D.QDiagram D.B D.V2 Double D.Any
    renderTick _ = tickToDiagram
    renderTickStatic _ = tickToDiagramStatic
    renderTicks proxya renderSettings ticks =
        foldMap (renderTick proxya renderSettings) ticks
    writeRepToFile _ path rep = do
        let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
        let svgDoc = D.renderDia D.SVG options rep
        let bs = Graphics.Svg.Core.renderBS svgDoc
        Data.ByteString.Lazy.writeFile path bs

tickToDiagram :: RenderSettings -> Tick -> D.Diagram D.B
tickToDiagram renderSettings@RenderSettings{ heightMultiplier, textMultiplier } tick =
    let staticTick = tickToDiagramStatic renderSettings tick
    in
    case _offset tick of
        Vertical y ->
            staticTick
                & D.translate (D.r2 (realToFrac $ _postPos tick, realToFrac y))
        Radial rad ->
            staticTick
                & D.translate (D.r2 (0, realToFrac rad))
                & D.rotateBy (negate $ realToFrac $ _postPos tick)

tickToDiagramStatic :: RenderSettings -> Tick -> D.Diagram D.B
tickToDiagramStatic RenderSettings{ heightMultiplier, textMultiplier } tick =
    let Tick { _prePos, _postPos, _info } = tick
        TickInfo { _start, _end, _mlabel } = _info
        startV2 = D.r2 (0, heightMultiplier * _start)
        endV2   = D.r2 (0, heightMultiplier * _end)
        diffV2  = endV2 - startV2
        tickDia :: D.Diagram D.B
        tickDia = laserline [diffV2] & D.translate (fmap realToFrac startV2)
        labelDia :: D.Diagram D.B
        labelDia = fromMaybe mempty $ do
            Label {..} <- _mlabel
            let labelOffset :: D.V2 InternalFloat
                labelOffset
                  = _anchorOffset * D.r2 (1, heightMultiplier)
                  + case _tickAnchor of
                      Pct p -> startV2 + diffV2 * D.V2 p p
                      FromTopAbs x -> endV2 + D.r2 (0, heightMultiplier * x)
                      FromBottomAbs x -> startV2 + D.r2 (0, heightMultiplier * x)
            pure $
                D.alignedText (realToFrac $ _xPct _textAnchor) (realToFrac $ _yPct _textAnchor) _text
                  & D.fontSizeL (realToFrac $ heightMultiplier * textMultiplier * _fontSize) & D.fc D.black
                  & D.font "Comfortaa"
                  & D.translate (fmap realToFrac labelOffset)
     in D.lc D.red tickDia <> labelDia

