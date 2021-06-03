module SlideRules.Utils where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.IO.Unsafe

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

(<<<) f g = f . g
infixr 2 <<<

-- Does not follow lens laws, (mayDef %~ id) /= id
mayDef :: Default a => Lens' (Maybe a) a
mayDef = lens (fromMaybe def) (\_ x -> Just x)

maybeM :: Monad m => b -> (a -> m b) -> Maybe a -> m b
maybeM b _ Nothing = pure b
maybeM _ f (Just a) = f a

uprint :: Monad m => String -> m ()
uprint s =
    unsafePerformIO $ do
        putStrLn s
        pure (pure ())

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
