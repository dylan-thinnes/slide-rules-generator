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
import qualified SlideRules.Renderer.FasterSVG as Faster
import qualified SlideRules.Renderer.Diagrams
import SlideRules.Renderer

writeToFasterSVG =
    writeScalesToFile (Proxy :: Proxy Faster.FasterSVG)

writeToDiagrams =
    writeScalesToFile (Proxy :: Proxy SlideRules.Renderer.Diagrams.Dias)

dumpToFile path scales = do
    writeFile path $ show $ generateScales scales

deepseqToFile path scales =
    writeFile path $ deepseq (generateScales scales) "success!"
