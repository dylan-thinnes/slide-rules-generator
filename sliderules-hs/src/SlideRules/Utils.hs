{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module SlideRules.Utils where

-- base
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.IO.Unsafe
import Numeric (showFFloat)
import Control.Monad.Fail

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

-- mtl
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- transformers
import Control.Monad.Trans.Maybe

-- local (sliderules)
import SlideRules.Types

-- MISC

asksGets :: (MonadReader r m, MonadState s m) => (r -> s -> a) -> m a
asksGets f = do
    env <- ask
    state <- get
    pure $ f env state

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

-- radians to degrees, degrees to radians
deg, rad :: InternalFloat -> InternalFloat
deg x = x * 180 / pi
rad x = x * pi / 180

-- DIAGRAMS
laserline :: [D.V2 Double] -> D.Diagram D.B
laserline positions = D.fromOffsets positions & D.lineWidth D.ultraThin

-- NUMBERS TO STRINGS

isIntegral :: InternalFloat -> Bool
isIntegral x = x == fromIntegral (floor x)

sigExp :: Integer -> (Integer, Int)
sigExp = go 0
    where
    go n i
      | i `mod` 10 == 0 = go (n + 1) (i `div` 10)
      | otherwise = (i, n)

intOrFraction :: InternalFloat -> Either Integer InternalFloat
intOrFraction x = if isIntegral x then Left (floor x) else Right x

showIOrF :: (Integer -> a) -> (InternalFloat -> a) -> InternalFloat -> a
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

-- ERRORS
newtype FailZero m a = FailZero { runFailZero :: (m a) }
mayFail = FailZero . Just
runMayFail = runMaybeT . runFailZero
runMayFail_ x = runMaybeT (runFailZero x) >> pure ()

deriving instance Functor m => Functor (FailZero m)
deriving instance Applicative m => Applicative (FailZero m)
deriving instance Monad m => Monad (FailZero m)
deriving instance MonadReader r m => MonadReader r (FailZero m)
deriving instance MonadState s m => MonadState s (FailZero m)
deriving instance MonadWriter w m => MonadWriter w (FailZero m)
instance MonadTrans FailZero where
    lift = FailZero
instance MonadPlus m => MonadFail (FailZero m) where
    fail _ = lift mzero
