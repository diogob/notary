module Main where

import Notary
import Protolude
import Servant.Elm
import Elm

spec :: Spec
spec = Spec ["Notary"]
            (defElmImports
             : generateElmForAPI  (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "elm-api"