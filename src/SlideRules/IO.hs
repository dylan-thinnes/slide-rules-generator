module SlideRules.IO where

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

writeToFile path diagram = do
    let options = D.SVGOptions (D.mkWidth 2000) Nothing (T.pack "") [] True
    let svgDoc = D.renderDia D.SVG options diagram
    let bs = Graphics.Svg.Core.renderBS svgDoc
    Data.ByteString.Lazy.writeFile path bs
