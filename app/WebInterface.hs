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

startServer :: IO ()
startServer = scotty 8081 $ do
    middleware $ staticPolicy $ prependRoot >-> indexHTMLPolicy

    post "/api/make" $ do
        serializableGen <- jsonData :: ActionM SerializableGenerator
        --let generator = interpretSerializableGen serializableGen
        WWW.text "Hello!"

prependRoot :: Policy
prependRoot = policy $ Just . ("web-interface/" ++)

indexHTMLPolicy :: Policy
indexHTMLPolicy = policy $ Just . traceShowId . f . traceShowId
    where
        f path
          | "" <- path = "index.html"
          | last path == '/' = path ++ "index.html"
          | otherwise = path
