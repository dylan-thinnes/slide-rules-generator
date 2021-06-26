module SlideRules.Transformations where

-- base
import Data.Monoid (Endo(..))
import Control.Monad

-- local (sliderules)
import SlideRules.Types
import SlideRules.Utils

data Transformation
    = Offset InternalFloat
    | Scale  InternalFloat
    | Log    InternalFloat
    | LogLog InternalFloat
    | Fold   InternalFloat InternalFloat
    | Rotate InternalFloat
    | Flip
    | Tan | Sin | Cos
    | Pow InternalFloat
    | Above InternalFloat
    | Below InternalFloat
    | Within InternalFloat InternalFloat
    deriving Show

runTransformations :: [Transformation] -> InternalFloat -> Maybe InternalFloat
runTransformations []     x = Just x
runTransformations (t:ts) x = runTransformations ts =<< runTransformation t x

runTransformation :: Transformation -> InternalFloat -> Maybe InternalFloat
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

foldTransformation :: (InternalFloat, InternalFloat) -> InternalFloat -> InternalFloat
foldTransformation (lower, upper) x
  | x < lower = upper + (x - lower)
  | upper < x = lower + (x - upper)
  | otherwise = x
