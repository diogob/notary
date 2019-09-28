{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Protolude hiding (get)

import Notary
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $
    describe "Public API" $ do
        describe "POST /salt" $ do
            it "responds with 415 when ContentType is not set" $
                post "/salt" "" `shouldRespondWith` 415
            it "responds with 400 when body is empty" $
                saltJSON "" `shouldRespondWith` 400
            it "responds with 400 when body does not have the right json shape" $
                saltJSON [json| { } |] `shouldRespondWith` 400
            it "responds with 200 and salt when body has the right shape" $
                saltJSON [json| { address: "test-address" } |] `shouldRespondWith` 200

        describe "POST /signup" $ do
            it "responds with 415 when ContentType is not set" $
                post "/signup" "" `shouldRespondWith` 415
            it "responds with 400 when body is empty" $
                signupJSON "" `shouldRespondWith` 400
            it "responds with 400 when body does not have the right json shape" $
                signupJSON [json| { } |] `shouldRespondWith` 400
            it "responds with 400 when jwt is empty" $
                signupJSON [json| { "jwt": "", "public_key": "" } |] `shouldRespondWith` 400
            it "responds with 400 when jwt is invalid" $
                signupJSON [json| { "jwt": "test", "publicKey": "" } |] `shouldRespondWith` 400
    where
        signupJSON = request methodPost "/signup" [("Content-Type", "application/json")]
        saltJSON = request methodPost "/salt" [("Content-Type", "application/json")]
        conf = Config { db = "postgres://notary_public@/notary", port = 8080 }