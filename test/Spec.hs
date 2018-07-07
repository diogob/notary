{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Protolude hiding (get)

import CoinberryApi
import Hasql.Pool (acquire)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app <$> acquire (10, 10, "postgres://localhost/coinberry_api_test")) $
    describe "GET /users" $ do
        it "responds with 200" $
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = [json|
                [{"userId":1,"userFirstName":"Isaac","userLastName":"Newton"},{"userId":2,"userFirstName":"Albert","userLastName":"Einstein"}]
            |]
            get "/users" `shouldRespondWith` users
    where
            app = serve api . server