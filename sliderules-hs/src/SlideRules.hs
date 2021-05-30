module SlideRules where

-- default
import Data.Default

-- local (sliderules)
import SlideRules.Generator
import SlideRules.Lenses
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils

ex100 :: Generator ()
ex100 =
    postTransform (Log 10) $ together $
        [ withInfo (\f x -> (f x) { mlabel = Just (def { fontSize = 10, text = showMax x }) }) $ do
            x <- list [1..9]
            output x
            preTransform (Offset x) $ preTransform (Scale 0.1) $ do
                x <- list [1..9]
                output x
        , withInfo (\f x -> (f x) { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "pi" }) })
            (output pi)
        , withInfo (\f x -> (f x) { start = 0.5, end = 0.6, mlabel = Just (def { fontSize = 10, text = "e" }) })
            (output e)
        ]
