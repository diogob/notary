module Main where

import Notary
import Protolude


import           System.Environment (getArgs)
import Servant.JS


main :: IO ()
main = writeJSForAPI api (axios defAxiosOptions) "./coinberry-api.js"

jsCode :: Text
jsCode = jsForAPI api vanillaJS
