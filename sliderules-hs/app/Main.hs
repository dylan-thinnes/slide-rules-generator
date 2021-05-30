module Main where

-- local (sliderules)
import SlideRules
import SlideRules.IO

main :: IO ()
main = writeToFile "circle.svg" total
