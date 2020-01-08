{-# LANGUAGE QuasiQuotes #-}
module SaltEndpointSpec (spec) where

import Protolude hiding (hash)

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

hexSha512 :: ByteString -> String
hexSha512 bs = show (hash bs :: Digest SHA512)

spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $
    describe "POST /salt" $ do
        it "responds with 415 when ContentType is not set" $
            post "/salt" "" `shouldRespondWith` 415
        it "responds with 400 when body is empty" $
            saltJSON "" `shouldRespondWith` 400
        it "responds with 400 when body does not have the right json shape" $
            saltJSON [json| { } |] `shouldRespondWith` 400
        it "responds with 200 and salt when body has the right shape" $
            saltJSON [json| { address: "test-address" } |] `shouldRespondWith` 200
    where
        saltJSON = request methodPost "/salt" [("Content-Type", "application/json")]
        conf = Config 
                { db = "postgres://notary_public:test@localhost/notary_test"
                , port = 8080
                , publicUri = "http://localhost:8080" 
                , confirmationUri = fromMaybe (panic "Could not parse test URI") $ parseURI "https://httpbin.org/post"
                }