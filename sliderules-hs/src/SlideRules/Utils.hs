module SlideRules.Utils where

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

-- lens
import Control.Lens

-- MISC

-- Does not follow lens laws, (mayDef %~ id) /= id
mayDef :: Default a => Lens' (Maybe a) a
mayDef = lens (fromMaybe def) (\_ x -> Just x)

-- MATH
loglogBase :: Floating a => a -> a -> a
loglogBase n = logBase n . logBase n

e :: Floating a => a
e = sum $ map (recip . fromIntegral . fac) [0..17]
    where
        fac :: Integer -> Integer
        fac n = product ([2..n] :: [Integer])


-- DIAGRAMS
laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black
