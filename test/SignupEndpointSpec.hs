{-# LANGUAGE QuasiQuotes #-}
module SignupEndpointSpec (spec) where

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

hexSha512 :: ByteString -> String
hexSha512 bs = show (hash bs :: Digest SHA512)
{-
Keypair used in specs
{
  "p": "3KMtptWP5IOci4guXWaGHtxf4STg6VKeXF6n6zSZA-vi7hyEMOLnqb7Ih2-IYJlDSIZKDmvQ_rYBCJn1yGAgfPUDTPNTgIPuJMkJbq1AWDnQfSh6yRwvGivK0uZXSxYEL7Aor3sJHCiDWs-S1d2UCgDgkotfgeDhBpEj_SUb8Tk",
  "kty": "RSA",
  "q": "usA_63h2apY6a2Ybslr7sYFdyiEkq7UCwMrXqWgEkbqaswuubkaPhiQD_akhjttjsB2aTMjAscKW5Qf6QTWbGMhG0SjAxw1RiaGuthyrwP8xhJ5yi1MedljmlTekVY4UtNQsqrKyNvQCy1Abf6zPWdFnXYz_kwnLVsO82c12u_s",
  "d": "A48C8uB1qp0Hbl-aPU9Vm46Eh_PnCx8tpdn1WVI59rGANoZmGdg-1zbkZMW7uNZtRiAoIn0R-Kepmad0BVhw77uhMTRSjU8_dnL077UEAviIlzjocOawvH9N9yGOWiT2BeG9iNCZVvmpL5k_eNqhNbhFX5jcuS5SpEsxN5uwPsgSupA6NHVfuZev7WtfdjRzUgIZX-I8JzedCautfvC5lYoaXe0MWkK5o7-9U1HYFSRoKr_V1XjRkFPVaCTF9WEf-zecmSZQQ83vJ1cED8g4M5hyctKJu5oUuNYykTaIU41qfpo7sbTKeOBdEw5xRhM3h0MNr62rtAu6v61_eE8cMQ",
  "e": "AQAB",
  "use": "sig",
  "kid": "test",
  "qi": "qAZQK3x2fEbSWTgUtDIUzyo7d9EVjEuw6OlnJPM1Vu78gja4DGOPm5XHOeYnHFuiw6M5vKSLj0Hs1vwUPTLxDwveI7BHBhdWYQtDjE2U9vxNNNmfbCs45fNXQ9YMRihaC3Est_GxsQxif8RLj5wkRt_mVpNHjSF6IPZOUcm6suI",
  "dp": "Vdk4Ph4Cqwt6sd__CmCF9yQQOm47BQ4ciBxcgewpTZlO5w85cDwZn7aYF_qRueRohaOa1RmmbDUtGceuFpUwju5Wpm5XIz4yPyXqAaJv0uRL9yb0_6NzwF8hwAXYGWMrpdKJQh5BYS9yTLdPjt90ZqAIwXxxYPu7Eu8ObcUupiE",
  "alg": "RS512",
  "dq": "bPPalus50ueAf8YbpIu2ShIagOGjPlYlnAr7lUctW0QOGyVWh9m8xFrgxS3WeTz_xPEQY3nACjVnyYlE9KRsryGxRzfKeJcJmg1tG6xQfBGmdyPl2ekNXahyJl5HDu8Mn48l3BTTKoJj2vAKiwtsEY-9CXRIayWMrzsVQ9C-U98",
  "n": "oPRApI4kOyHApTIbXnHwnDOarCgarpe4zkkAZxoysO6rYL-f7r9IKgBIuvFsZtmueXNp8QTZ93R5E8wAJB9_aZL6q6QWMiPTEkWANKPnQ2iwoWQdS0lBZwIGb-0uaYeJxxG9qzhi3KpXf8_H1fg81mPWSB_bnOTwCBkAUW4KrvgkIg0RGdTfb_ParpAuEbONT65dtfa_gugBTEC20agZ2VFElc8G7IbktO5HGCnrCVI4-hvvT-Rg_mfTexntWBaisnDGynDAyUoY_9ycAfo6wgtg4AMopPqVzdJW4CowHW0sml6XeUHOBHenK27KPI_Xc1VbHKy1dn-hPwOroOAl4w"
}

public key only:
{
  "kty": "RSA",
  "e": "AQAB",
  "use": "sig",
  "kid": "test",
  "alg": "RS512",
  "n": "oPRApI4kOyHApTIbXnHwnDOarCgarpe4zkkAZxoysO6rYL-f7r9IKgBIuvFsZtmueXNp8QTZ93R5E8wAJB9_aZL6q6QWMiPTEkWANKPnQ2iwoWQdS0lBZwIGb-0uaYeJxxG9qzhi3KpXf8_H1fg81mPWSB_bnOTwCBkAUW4KrvgkIg0RGdTfb_ParpAuEbONT65dtfa_gugBTEC20agZ2VFElc8G7IbktO5HGCnrCVI4-hvvT-Rg_mfTexntWBaisnDGynDAyUoY_9ycAfo6wgtg4AMopPqVzdJW4CowHW0sml6XeUHOBHenK27KPI_Xc1VbHKy1dn-hPwOroOAl4w"
}
-}
spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $
    describe "POST /signup" $ do
        it "responds with 415 when ContentType is not set" $
            post "/signup" "" `shouldRespondWith` 415
        it "responds with 400 when body is empty" $
            signupJSON "" `shouldRespondWith` 400
        it "responds with 400 when body does not have the right json shape" $
            signupJSON [json| { } |] `shouldRespondWith` 400
        it "responds with 400 when jwt is empty" $
            signupJSON [json| { "jwt": "", "publicKey": {
                "kty": "oct",
                "use": "sig",
                "kid": "test",
                "k": "3rAoCHTkk3NTLJC_sPn8GL7QsXBUjzE3gRdE28rXiOkl01l9E217i4MY9JBeJFUFprEhw9j7oRcLFHjwvOj6hLL5TFnkFabYWXHrmHG4nnpahzl0gEaGR4ysU0IF3VF97T1NbqOoMhJ51SBrhAWhLIQUdfCsMtjcjjnuAOf86TXF_F8svWhZtIlzhvT7lopH3nBReJC02lUbgdi2M-P-lOyzXK8ooZ6Cc4ZP1D2xkKpGOnrWkGubbyIPw36dP1Uyb-KUDoYLQvvAmH3yHokAzx0mo1dk35gN0VuySHLavc3f4MHNVyyFOMb8zwqXnxFsCxVaoDgNsdGWSySn3X-dXA",
                "alg": "HS256"
                } } |] `shouldRespondWith` 400
        it "responds with 400 when jwt is invalid" $
            signupJSON [json| { "jwt": "test", "publicKey": {
                "kty": "oct",
                "use": "sig",
                "kid": "test",
                "k": "3rAoCHTkk3NTLJC_sPn8GL7QsXBUjzE3gRdE28rXiOkl01l9E217i4MY9JBeJFUFprEhw9j7oRcLFHjwvOj6hLL5TFnkFabYWXHrmHG4nnpahzl0gEaGR4ysU0IF3VF97T1NbqOoMhJ51SBrhAWhLIQUdfCsMtjcjjnuAOf86TXF_F8svWhZtIlzhvT7lopH3nBReJC02lUbgdi2M-P-lOyzXK8ooZ6Cc4ZP1D2xkKpGOnrWkGubbyIPw36dP1Uyb-KUDoYLQvvAmH3yHokAzx0mo1dk35gN0VuySHLavc3f4MHNVyyFOMb8zwqXnxFsCxVaoDgNsdGWSySn3X-dXA",
                "alg": "HS256"
                } } |] `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and signed with the wrong key" $
            signupJSON [json| 
            { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.POstGetfAytaZS82wHcjoTyoqhMyxXiWdR7Nn7A29DNSl0EiXLdwJ6xC6AfgZWF1bOsS_TuYI3OG85AmiExREkrS6tDfTQ2B3WXlrr-wp5AokiRbz3_oB4OxG-W9KcEEbDRcZc0nH3L7LzYptiy1PtAylQGxHTWZXtGz4ht0bAecBgmpdgXMguEIcoqPJ1n3pIWk_dUZegpqx0Lka21H6XxUTxiy8OcaarA8zdnPUnV6AmNP3ecFawIFYdvJB_cm-GvpCSbr8G8y_Mllj8f4x9nBH8pQux89_6gUY618iYv7tuPWBFfEbLxtF2pZS6YC1aSfLQxeNe8djT9YjpvRZA"
            , "publicKey": { "kty": "RSA"
                            , "e": "AQAB"
                            , "use": "sig"
                            , "kid": "test"
                            , "alg": "RS256"
                            , "n": "kNFqxM_946-tllJmTr1zjye-Ltavmk-QB1pveHS6mjg4JvAazBJowUr1ySqgGj6Y0GVR5zarAZASLsEZfgb4Oa0GZmlRZTtd-o0asQQQexDEFShkSsa7S0w5EJXQovc0p7b4e45dv87LmWgAbBVv6lrBemjZnwg1u2gDoBEo-RLtgMagYexW9oSaF91cpXzQsGvPccPotmlxM0nQLvWhq3wwzIrPkelds-ZFJVOfWPaHax7v7vtRY2tuW-mesqEQlDuJpv4IDJTV05OKDyu-5L0QSCVDnWqXBup_PzyYHTgwoh19IOR3RCxSbyvHdLLj_xjZH1sAjLER4wOPtM165Q"
                            }
            }
            |] `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and correctly signed but missing kid" $
            signupJSON [json| 
                { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMiwiYXVkIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwIn0.PcLDqi9SWA0DsZ_wquguWyvL32cpry5WpjxIM1tZoEcxgVcTEa3F_kDvqQvgF_r2ev2zfoVGrf8Lknh81--hRpczPmiUdkRYT2P0njN2uqqFoOpXRQuerWZGtEvpmaX0qaNSPHoSVpkukhrhI3aslL7KCCX33DssoNBu7aYcBn1McNoiW4ZazPJG27Ipwfk-f7fI2MndLrLacguFAE7c3tb7qrSzze03QegA-kKlQTFLwxqjDiAWJORPAlzYZ0Ghk_IXFRZP57F0xMcuKQ82rLKmBZLYA3uNysfzNlfUaakHMqKWql329ne76ssrJIJ-l6GAgM6DgGMzj4mUNT2uXQ"
                , "publicKey": { "kty": "RSA"
                               , "e":"AQAB"
                               , "n":"nzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA-kzeVOVpVWwkWdVha4s38XM_pa_yr47av7-z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr_Mrm_YtjCZVWgaOYIhwrXwKLqPr_11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e-lf4s4OxQawWD79J9_5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa-GSYOD2QU68Mb59oSk2OB-BtOLpJofmbGEGgvmwyCI9Mw"
                               }
                }
                |] `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and correctly signed but has wrong kid" $
            signupJSON [json| 
                { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWF0IjoxNTE2MjM5MDIyLCJhdWQiOiJodHRwOi8vbG9jYWxob3N0OjgwODAifQ.IzxHrqVFP0aEkV9QuW3l-HV59TDWCa61Ypt9Pr_qraTDbYVSLIXOuZ6NncQM3tipIkTOoNL1OM4_RwQSOKtvK1DAIS5Eyn9vPBXL3DknauXyg2oSM8TgH5eNQkadmk_DzM_i0irT5M_yekWX_dfkuIcaRaYBv5GDEmNx7z4DX-ox-TBGIYe60mzDOzPRvctMEjDKlXMqxwh4MQVTgUoxTiS0YL6p2T3S9LfEI1UTiI_KPw2EqFL-E3hF5q8TeQKjEuXwD5i9wK0ALynfzepd_tD9I2Rt0Z9gj90c9R2xVHY03GthSuLIEKba47ejmCmjCihB2XkqwrgnM2QPVpXOOg"
                , "publicKey": #{publicKey "7bbdb027-6a08-4663-890d-b04fb43cf9fa"}
                }
                |] `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and correctly signed but has an empty aud claim" $
            signupJSONWithRightKey "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWF0IjoxNTE2MjM5MDIyfQ.HwSLQVjY53dYYuek6t2sErE3HLc4iF_V13sYyDuqqQx6v3kFVmHhMC0OYM4Uu1gjanIxmXss7eqCfgXwqFt7OTIF4LFCnuY8dNEXRR58j0mMKa3VuFGXH2niJ3Wv8UQWTbNw7LcLFkZanYtNF7HoyOOcKro4lH7hdZ5Etx6d8VnDLxDZg1xImbOe5L-boNb8hlT13h7cvaMCC5sLWp6yJcltx8fQ-yPsiQHS7pE_Qw_L8F0eeVBLc5iF2boPsAyMQoStAD9kyFQB04UNFQ7uzdSZH_r52h9-nTxfme96JELvbEcLVDR_QrRjxgGo8H2OJLrVUeymd_UsvZKhLoOtfw"
            `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and correctly signed but has the invalid aud claim" $
            signupJSONWithRightKey "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWF0IjoxNTE2MjM5MDIyLCJhdWQiOiJodHRwOi8vc29tZW90aGVyaG9zdC5jb20ifQ.fBGryWdxh8kMHJbx_bvKmJZjIWsyus4v69qJZvVvOayrYZiYdSkEGU_Mwg-vby05f8cvX2wJQoMda_CYi9f9Fiw_7QtuFbWbmqycknzn3U_FdHF-d5N_-FAvWwv9UYfceNKFDN2Onv3ja3ledi7g9YddGQEGrewtNi8j2aESnNMc9eBVCgdn161T-LbSvkbG1Nyyd5Fnd_wTzflfyH9fpp5s81nLsPltrI-ab_tO8rtz0mLzlUJFa75oKRMdsWXxhaVrrcfDxXnLiBBkLsysxkpbXy6FfzUKQwafX-zMFveqVoVdj_DwZAq0mb99bi1QGIO8_ohab3xJ5OJB-SM3ig"
            `shouldRespondWith` 400
        it "responds with 200 when jwt is valid and correctly signed" $
            signupJSONWithRightKey "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwiaWF0IjoxNTE2MjM5MDIyLCJhdWQiOiJodHRwOi8vbG9jYWxob3N0OjgwODAifQ.IzxHrqVFP0aEkV9QuW3l-HV59TDWCa61Ypt9Pr_qraTDbYVSLIXOuZ6NncQM3tipIkTOoNL1OM4_RwQSOKtvK1DAIS5Eyn9vPBXL3DknauXyg2oSM8TgH5eNQkadmk_DzM_i0irT5M_yekWX_dfkuIcaRaYBv5GDEmNx7z4DX-ox-TBGIYe60mzDOzPRvctMEjDKlXMqxwh4MQVTgUoxTiS0YL6p2T3S9LfEI1UTiI_KPw2EqFL-E3hF5q8TeQKjEuXwD5i9wK0ALynfzepd_tD9I2Rt0Z9gj90c9R2xVHY03GthSuLIEKba47ejmCmjCihB2XkqwrgnM2QPVpXOOg"
            `shouldRespondWith` 200
    where
        publicKey :: Text -> Value
        publicKey kid = [aesonQQ|{
            "kty": "RSA",
            "e": "AQAB",
            "use": "sig",
            "kid": #{kid},
            "alg": "RS512",
            "n": "oPRApI4kOyHApTIbXnHwnDOarCgarpe4zkkAZxoysO6rYL-f7r9IKgBIuvFsZtmueXNp8QTZ93R5E8wAJB9_aZL6q6QWMiPTEkWANKPnQ2iwoWQdS0lBZwIGb-0uaYeJxxG9qzhi3KpXf8_H1fg81mPWSB_bnOTwCBkAUW4KrvgkIg0RGdTfb_ParpAuEbONT65dtfa_gugBTEC20agZ2VFElc8G7IbktO5HGCnrCVI4-hvvT-Rg_mfTexntWBaisnDGynDAyUoY_9ycAfo6wgtg4AMopPqVzdJW4CowHW0sml6XeUHOBHenK27KPI_Xc1VbHKy1dn-hPwOroOAl4w"
          }|]

        -- define request helpers in a list
        [saltJSON, signupJSON] = flip (request methodPost) [("Content-Type", "application/json")] <$> ["/salt", "/signup"]
        signupJSONWithRightKey :: Text -> WaiSession SResponse
        signupJSONWithRightKey jwt = do
            r <- saltJSON [json| { address: "1234567890" } |]
            let rBody :: LByteString
                rBody = simpleBody r
                mSalt :: Maybe ByteString
                mSalt = toS <$> rBody ^? key "salt" . _String
            case mSalt of
                Just s -> signupJSON [json| 
                                        { "jwt": #{jwt}
                                        , "publicKey": #{publicKey $ toS $ hexSha512("1234567890 " <> toS s)}
                                        }
                                        |]
                Nothing -> panic "Could not fetch salt during test setup"
        conf = Config 
                { db = "postgres://notary_public:test@localhost/notary_test"
                , port = 8080
                , publicUri = "http://localhost:8080" 
                , confirmationUri = fromMaybe (panic "Could not parse test URI") $ parseURI "https://httpbin.org/post"
                }