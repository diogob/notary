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
        it "responds with 200 when jwt is valid even when confirmation fails" $
            confirmJSONWithRightKey "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWF0IjoxNTE2MjM5MDIyLCJhdWQiOiJodHRwOi8vbG9jYWxob3N0OjgwODAifQ.IzxHrqVFP0aEkV9QuW3l-HV59TDWCa61Ypt9Pr_qraTDbYVSLIXOuZ6NncQM3tipIkTOoNL1OM4_RwQSOKtvK1DAIS5Eyn9vPBXL3DknauXyg2oSM8TgH5eNQkadmk_DzM_i0irT5M_yekWX_dfkuIcaRaYBv5GDEmNx7z4DX-ox-TBGIYe60mzDOzPRvctMEjDKlXMqxwh4MQVTgUoxTiS0YL6p2T3S9LfEI1UTiI_KPw2EqFL-E3hF5q8TeQKjEuXwD5i9wK0ALynfzepd_tD9I2Rt0Z9gj90c9R2xVHY03GthSuLIEKba47ejmCmjCihB2XkqwrgnM2QPVpXOOg" `shouldRespondWith` 200
        where
        -- define request helpers in a list
        [saltJSON, signupJSON, confirmJSON] = flip (request methodPost) [("Content-Type", "application/json")] <$> ["/salt", "/signup", "/confirm"]

        publicKey :: Text -> Value
        publicKey kid = [aesonQQ|{
            "kty": "RSA",
            "e": "AQAB",
            "use": "sig",
            "kid": #{kid},
            "alg": "RS512",
            "n": "oPRApI4kOyHApTIbXnHwnDOarCgarpe4zkkAZxoysO6rYL-f7r9IKgBIuvFsZtmueXNp8QTZ93R5E8wAJB9_aZL6q6QWMiPTEkWANKPnQ2iwoWQdS0lBZwIGb-0uaYeJxxG9qzhi3KpXf8_H1fg81mPWSB_bnOTwCBkAUW4KrvgkIg0RGdTfb_ParpAuEbONT65dtfa_gugBTEC20agZ2VFElc8G7IbktO5HGCnrCVI4-hvvT-Rg_mfTexntWBaisnDGynDAyUoY_9ycAfo6wgtg4AMopPqVzdJW4CowHW0sml6XeUHOBHenK27KPI_Xc1VbHKy1dn-hPwOroOAl4w"
          }|]

        hexSha512 :: ByteString -> String
        hexSha512 bs = show (hash bs :: Digest SHA512)

        confirmJSONWithRightKey :: Text -> WaiSession SResponse
        confirmJSONWithRightKey jwt = do
            r <- saltJSON [json| { address: "1234567890" } |]
            let rBody :: LByteString
                rBody = simpleBody r
                mSalt :: Maybe ByteString
                mSalt = toS <$> rBody ^? key "salt" . _String
            case mSalt of
                Just s -> confirmJSON [json| 
                                        { "jwt": #{jwt}
                                        , "kid": #{hexSha512("1234567890 " <> toS s)}
                                        }
                                        |]
                Nothing -> panic "Could not fetch salt during test setup"

        conf = Config 
                { db = "postgres://notary_public:test@localhost/notary_test"
                , port = 8080
                , publicUri = "http://localhost:8080" 
                , confirmationUri = fromMaybe (panic "Could not parse test URI") $ parseURI "https://httpbin.org/post"
                }