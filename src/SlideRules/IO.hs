module SlideRules.IO where

-- bytestring
import qualified Data.ByteString.Lazy

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
import SlideRules.FastSVG

writeToFastSVG path scale = do
    let svg = (foldMap . foldMap) (tickToElement (heightMultiplier scale) (textMultiplier scale)) (generateScales scale)
    writeFile path $ show svg

writeToFile path diagram = do
    let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
    let svgDoc = D.renderDia D.SVG options diagram
    let bs = Graphics.Svg.Core.renderBS svgDoc
    Data.ByteString.Lazy.writeFile path bs

dumpToFile path scales = do
    writeFile path $ show $ generateScales scales

deepseqToFile path scales =
    writeFile path $ deepseq (generateScales scales) "success!"
