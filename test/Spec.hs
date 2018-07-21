{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Protolude hiding (get)

import CoinberryApi
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app <$> acquire (10, 10, "postgres://localhost/coinberry_test")) $
    describe "GET /currencies" $ do
        it "responds with 200" $
            get "/currencies" `shouldRespondWith` 200
        it "responds with [Currency]" $ do
            let currencies = [json|
                []
            |]
            get "/currencies" `shouldRespondWith` currencies
    where
            app = serve (Proxy :: Proxy API) . server