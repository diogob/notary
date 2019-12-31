{-# LANGUAGE QuasiQuotes #-}
module ConfirmEndpointSpec (spec) where

import Protolude hiding (get, hash)

import Notary
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method
import Network.Wai.Test hiding (request)
import Data.Aeson (Value, decode)
import Data.Aeson.Lens
import Control.Lens
import Crypto.Hash
import qualified Data.ByteString.Base64 as B64
import Data.String (String)
import Network.URI
import Data.Aeson.QQ

spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $
    describe "POST /confirm" $ do
        it "responds with 415 when ContentType is not set" $
            post "/confirm" "" `shouldRespondWith` 415
        it "responds with 400 when body is empty" $
            confirmJSON "" `shouldRespondWith` 400
        it "responds with 400 when body does not have the right json shape" $
            confirmJSON [json| { } |] `shouldRespondWith` 400
        it "responds with 400 when jwt is empty" $
            confirmJSON [json| { "jwt": "" } |] `shouldRespondWith` 400
    where
        -- define request helpers in a list
        confirmJSON = request methodPost "/confirm" [("Content-Type", "application/json")]
        conf = Config 
                { db = "postgres://notary_public:test@localhost/notary_test"
                , port = 8080
                , publicUri = "http://localhost:8080" 
                , confirmationUri = fromMaybe (panic "Could not parse test URI") $ parseURI "https://httpbin.org/post"
                }