{-# LANGUAGE TypeApplications #-}
module SlideRules.Utils where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.IO.Unsafe
import Numeric (showFFloat)

-- Decimal
import Data.Decimal

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

-- local (sliderules)
import SlideRules.Types

-- MISC

(<<<) f g = f . g
infixr 2 <<<

mayDef :: Default a => (a -> Maybe b) -> Maybe a -> Maybe b
mayDef f ma = f $ fromMaybe def ma

-- Does not follow lens laws, (mayDef %~ id) /= id
mayDefL :: Default a => Lens' (Maybe a) a
mayDefL = lens (fromMaybe def) (\_ x -> Just x)

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

slogBase :: InternalFloat -> InternalFloat -> InternalFloat
slogBase n x = realToFrac $ logBase (realToFrac n) (realToFrac x)

sloglogBase :: InternalFloat -> InternalFloat -> InternalFloat
sloglogBase n x = realToFrac $ loglogBase (realToFrac n) (realToFrac x)

e :: Floating a => a
e = sum $ map (recip . fromIntegral . fac) [0..17]
    where
        fac :: Integer -> Integer
        fac n = product ([2..n] :: [Integer])


-- DIAGRAMS
laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin & D.lc D.black

-- NUMBERS TO STRINGS

isIntegral :: InternalFloat -> Bool
isIntegral x = x == fromIntegral (floor x)

sigExp :: Integer -> Maybe (Integer, Int)
sigExp = go 0
    where
    go n i
      | i `mod` 10 == 0 = go (n + 1) (i `div` 10)
      | otherwise = Just (i, n)

intOrFraction :: InternalFloat -> Either Integer InternalFloat
intOrFraction x = if isIntegral x then Left (floor x) else Right x

showIOrF :: (Integer -> String) -> (InternalFloat -> String) -> InternalFloat -> String
showIOrF showInt showFloat = either showInt showFloat . intOrFraction

showPrecision, showP :: Int -> InternalFloat -> String
showPrecision n f = showFFloat (Just n) (realToFrac f) ""
showP = showPrecision

showMax, showM :: InternalFloat -> String
showMax f = showFFloat Nothing (realToFrac f) ""
showM = showMax

showFunction, showF :: Show a => (InternalFloat -> a) -> InternalFloat -> String
showFunction f = show . f
showF = showFunction

showInt, showI :: InternalFloat -> String
showInt = showF floor
showI = showInt
