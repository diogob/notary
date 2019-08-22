{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Protolude hiding (get)

import Notary
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $
    describe "POST /signup" $ do
        it "responds with 415 when body is empty" $
            post "/signup" "" `shouldRespondWith` 415
        it "responds with 415 when jwt is empty" $
            post "/signup" [json| { "jwt": "" } |] `shouldRespondWith` 415
        it "responds with 415 when jwt is invalid" $
            post "/signup" [json| { "jwt": "test" } |] `shouldRespondWith` 415
    where
        conf = Config { db = "postgres://localhost/coinberry_test", port = 8080 }