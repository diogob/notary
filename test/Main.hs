module Main where

import CoinberryApi
import Protolude
import Servant.Elm
import Elm

spec :: Spec
spec = Spec ["Generated", "Coinberry API"]
            (defElmImports
             : generateElmForAPI  (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "elm-api"