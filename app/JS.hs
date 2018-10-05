{-# LANGUAGE DeriveGeneric #-}

module Main where

import CoinberryApi
import Protolude


import           System.Environment (getArgs)
import Servant.JS


main :: IO ()
main = writeJSForAPI api (axios defAxiosOptions) "./coinberry-api.js"

jsCode :: Text
jsCode = jsForAPI api vanillaJS
