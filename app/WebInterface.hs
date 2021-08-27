{-# LANGUAGE OverloadedStrings #-}
module Main where

-- scotty
import Web.Scotty as WWW

-- wai-middleware-static
import Network.Wai.Middleware.Static

-- local (sliderules)
import SlideRules.SerializableGenerator

import Debug.Trace

main :: IO ()
main = startServer
