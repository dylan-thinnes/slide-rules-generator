module SlideRules.IO where

-- base
import System.IO

-- bytestring
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder as Builder

-- diagrams-*
import qualified Diagrams.Backend.SVG         as D
import qualified Diagrams.Backend.SVG.CmdLine as D
import qualified Diagrams.Prelude             as D
import qualified Diagrams.TwoD.Text           as D
import qualified Diagrams.TwoD.Vector         as D

-- deepseq
import Control.DeepSeq

-- svg-builder
import qualified Graphics.Svg.Core

-- text
import qualified Data.Text                    as T

-- local (sliderules)
import SlideRules.Scales
import qualified SlideRules.FastSVG as Fast
import qualified SlideRules.FasterSVG as Faster

writeToFasterSVG path scale = do
    let ticks = generateScales scale
    let viewbox = Faster.allTicksViewbox (renderSettings scale) $ (foldMap . foldMap) (:[]) ticks
    let content = (foldMap . foldMap) (Faster.tickToElement (renderSettings scale)) ticks
    withFile path WriteMode $ \handle -> do
        hSetBinaryMode handle True
        hSetBuffering handle $ BlockBuffering Nothing
        Builder.hPutBuilder handle $ Faster.svg (renderSettings scale) viewbox content

writeToFastSVG path scale = do
    let ticks = generateScales scale
    let content = (foldMap . foldMap) (Fast.tickToElement (heightMultiplier $ renderSettings scale) (textMultiplier $ renderSettings scale)) ticks
    writeFile path $ show $ Fast.svg content

writeToFile path diagram = do
    let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
    let svgDoc = D.renderDia D.SVG options diagram
    let bs = Graphics.Svg.Core.renderBS svgDoc
    Data.ByteString.Lazy.writeFile path bs

dumpToFile path scales = do
    writeFile path $ show $ generateScales scales

deepseqToFile path scales =
    writeFile path $ deepseq (generateScales scales) "success!"
