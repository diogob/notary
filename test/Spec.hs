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
spec = with (mkApp <$> (AppCtx <$> mkLogger <*> acquire (10, 10, "postgres://localhost/coinberry_test"))) $
    describe "POST /signup" $ do
        it "responds with 415 when body is empty" $
            post "/signup" "" `shouldRespondWith` 415