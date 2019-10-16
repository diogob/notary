{-# LANGUAGE QuasiQuotes #-}
module ApiSpec (spec) where

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

hexSha512 :: ByteString -> String
hexSha512 bs = show (hash bs :: Digest SHA512)

spec :: Spec
spec = with (mkApp <$> (AppCtx conf <$> mkLogger <*> acquire (10, 10, toS $ db conf) <*> mkGetTime)) $ do
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
            signupJSON [json| { "jwt": "", "public_key": {
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
        it "responds with 400 when jwt is valid and correctly signed but has an empty aud claim" $
            signupJSON [json| 
                { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.POstGetfAytaZS82wHcjoTyoqhMyxXiWdR7Nn7A29DNSl0EiXLdwJ6xC6AfgZWF1bOsS_TuYI3OG85AmiExREkrS6tDfTQ2B3WXlrr-wp5AokiRbz3_oB4OxG-W9KcEEbDRcZc0nH3L7LzYptiy1PtAylQGxHTWZXtGz4ht0bAecBgmpdgXMguEIcoqPJ1n3pIWk_dUZegpqx0Lka21H6XxUTxiy8OcaarA8zdnPUnV6AmNP3ecFawIFYdvJB_cm-GvpCSbr8G8y_Mllj8f4x9nBH8pQux89_6gUY618iYv7tuPWBFfEbLxtF2pZS6YC1aSfLQxeNe8djT9YjpvRZA"
                , "publicKey": { "kty":"RSA"
                                , "e":"AQAB"
                                , "kid":"a10ae964-228d-4c92-b0a7-f055d5b3ab2d"
                                , "n":"nzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA-kzeVOVpVWwkWdVha4s38XM_pa_yr47av7-z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr_Mrm_YtjCZVWgaOYIhwrXwKLqPr_11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e-lf4s4OxQawWD79J9_5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa-GSYOD2QU68Mb59oSk2OB-BtOLpJofmbGEGgvmwyCI9Mw"
                                }
                }
                |] `shouldRespondWith` 400
        it "responds with 400 when jwt is valid and correctly signed but has the wrong aud claim" $
            signupJSON [json| 
                { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMiwiYXVkIjoid3JvbmcgYXVkaWVuY2UifQ.A-eFDJ7i_hFZOsJkfQRAeB54gFIFArrDUdxndZB_y9Awfndbq67l3rbTsWLpfr2pgKoSazZ8edEZUBp2XNz4Ocs-5x9dfwtvTqum-IMEqlQbRU9okDIeKvcDICi1G-aHUUkya37ZK6C-4xPcNgm07Zc3S0rQIr0x3FrBIVpm0CPmqYp6eU-wTu3BHMB4uL8_DZCUBb1vSVYnRUMzU3Fa2AMooWOtDfTONrUg2bXNt4iiWSOUTpEWrhwtJw0jvASF2-sBFy2TdV2wSks48BfguxF6Wj9ldIrUEPgxN24mMuOfVk6IA0eYNLYdUN5wADcNUxLJOG8tTsgIV0-FkSZkNg"
                , "publicKey": { "kty": "RSA"
                               , "e":"AQAB"
                               , "kid":"7bbdb027-6a08-4663-890d-b04fb43cf9fa"
                               , "n":"nzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA-kzeVOVpVWwkWdVha4s38XM_pa_yr47av7-z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr_Mrm_YtjCZVWgaOYIhwrXwKLqPr_11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e-lf4s4OxQawWD79J9_5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa-GSYOD2QU68Mb59oSk2OB-BtOLpJofmbGEGgvmwyCI9Mw"
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
                { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMiwiYXVkIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwIn0.PcLDqi9SWA0DsZ_wquguWyvL32cpry5WpjxIM1tZoEcxgVcTEa3F_kDvqQvgF_r2ev2zfoVGrf8Lknh81--hRpczPmiUdkRYT2P0njN2uqqFoOpXRQuerWZGtEvpmaX0qaNSPHoSVpkukhrhI3aslL7KCCX33DssoNBu7aYcBn1McNoiW4ZazPJG27Ipwfk-f7fI2MndLrLacguFAE7c3tb7qrSzze03QegA-kKlQTFLwxqjDiAWJORPAlzYZ0Ghk_IXFRZP57F0xMcuKQ82rLKmBZLYA3uNysfzNlfUaakHMqKWql329ne76ssrJIJ-l6GAgM6DgGMzj4mUNT2uXQ"
                , "publicKey": { "kty": "RSA"
                               , "e":"AQAB"
                               , "kid":"7bbdb027-6a08-4663-890d-b04fb43cf9fa"
                               , "n":"nzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA-kzeVOVpVWwkWdVha4s38XM_pa_yr47av7-z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr_Mrm_YtjCZVWgaOYIhwrXwKLqPr_11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e-lf4s4OxQawWD79J9_5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa-GSYOD2QU68Mb59oSk2OB-BtOLpJofmbGEGgvmwyCI9Mw"
                               }
                }
                |] `shouldRespondWith` 400
        it "responds with 200 when jwt is valid and correctly signed" $ do
            r <- saltJSON [json| { address: "1234567890" } |]
            let rBody :: LByteString
                rBody = simpleBody r
                mSalt :: Maybe ByteString
                mSalt = toS <$> rBody ^? key "salt" . _String
            case mSalt of
                Just s -> do 
                                putErrLn $ "salt: " <> s
                                signupJSON [json| 
                                        { "jwt": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMiwiYXVkIjoiaHR0cDovL2xvY2FsaG9zdDo4MDgwIn0.PcLDqi9SWA0DsZ_wquguWyvL32cpry5WpjxIM1tZoEcxgVcTEa3F_kDvqQvgF_r2ev2zfoVGrf8Lknh81--hRpczPmiUdkRYT2P0njN2uqqFoOpXRQuerWZGtEvpmaX0qaNSPHoSVpkukhrhI3aslL7KCCX33DssoNBu7aYcBn1McNoiW4ZazPJG27Ipwfk-f7fI2MndLrLacguFAE7c3tb7qrSzze03QegA-kKlQTFLwxqjDiAWJORPAlzYZ0Ghk_IXFRZP57F0xMcuKQ82rLKmBZLYA3uNysfzNlfUaakHMqKWql329ne76ssrJIJ-l6GAgM6DgGMzj4mUNT2uXQ"
                                        , "publicKey": { "kty": "RSA"
                                                    , "e":"AQAB"
                                                    , "kid":#{hexSha512("1234567890 " <> toS s)}
                                                    , "n":"nzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA-kzeVOVpVWwkWdVha4s38XM_pa_yr47av7-z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr_Mrm_YtjCZVWgaOYIhwrXwKLqPr_11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e-lf4s4OxQawWD79J9_5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa-GSYOD2QU68Mb59oSk2OB-BtOLpJofmbGEGgvmwyCI9Mw"
                                                    }
                                        }
                                        |] `shouldRespondWith` 200

                Nothing -> panic "Could not fetch salt during test setup"
    where
        signupJSON = request methodPost "/signup" [("Content-Type", "application/json")]
        saltJSON = request methodPost "/salt" [("Content-Type", "application/json")]
        conf = Config { db = "postgres://notary_public:test@localhost/notary_test", port = 8080, publicUri = "http://localhost:8080" }