module SlideRules.Transformations where

-- base
import Data.Monoid (Endo(..))
import Control.Monad

-- local (sliderules)
import SlideRules.Types
import SlideRules.Utils

data Transformation fl
    = Offset fl
    | Scale  fl
    | Log    fl
    | LogLog fl
    | Fold   fl fl
    | Rotate fl
    | Flip
    | Tan | Sin | Cos
    | Pow fl
    | Above fl
    | Below fl
    | Within fl fl
    deriving Show

runTransformations :: (Ord fl, Num fl, Floating fl, Fractional fl) => [Transformation fl] -> fl -> Maybe fl
runTransformations []     x = Just x
runTransformations (t:ts) x = runTransformations ts =<< runTransformation t x

runTransformation :: (Ord fl, Num fl, Floating fl, Fractional fl) => Transformation fl -> fl -> Maybe fl
runTransformation (Offset offset)      x = pure $ offset + x
runTransformation (Scale scale)        x = pure $ scale * x
runTransformation (Log base)           x = pure $ slogBase base x
runTransformation (LogLog base)        x = pure $ sloglogBase base x
runTransformation (Fold lower upper)   x = pure $ foldTransformation (lower, upper) x
runTransformation (Rotate point)       x = pure $ if x >= point then x - point else (1 - (point - x))
runTransformation Flip                 x = pure $ 1 - x
runTransformation Tan                  x = pure $ tan $ rad x
runTransformation Sin                  x = pure $ sin $ rad x
runTransformation Cos                  x = pure $ cos $ rad x
runTransformation (Pow exp)            x = pure $ x ** exp
runTransformation (Above lower)        x = x <$ guard (lower <= x)
runTransformation (Below upper)        x = x <$ guard (x <= upper)
runTransformation (Within lower upper) x = runTransformation (Above lower) x >>= runTransformation (Below upper)

foldTransformation :: (Ord fl, Num fl) => (fl, fl) -> fl -> fl
foldTransformation (lower, upper) x
  | x < lower = upper + (x - lower)
  | upper < x = lower + (x - upper)
  | otherwise = x
