module Notary exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


postSalt : SaltRequest -> Http.Request (Salt)
postSalt body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "salt"
                ]
        , body =
            Http.jsonBody (encodeSaltRequest body)
        , expect =
            Http.expectJson decodeSalt
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSignup : SignupRequest -> Http.Request (NoContent)
postSignup body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "signup"
                ]
        , body =
            Http.jsonBody (encodeSignupRequest body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postConfirm : JwtBody -> Http.Request (NoContent)
postConfirm body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "confirm"
                ]
        , body =
            Http.jsonBody (encodeJwtBody body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

patchSignature : JwtBody -> Http.Request (NoContent)
patchSignature body =
    Http.request
        { method =
            "PATCH"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "signature"
                ]
        , body =
            Http.jsonBody (encodeJwtBody body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSignature : JwtBody -> Http.Request (NoContent)
deleteSignature body =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "signature"
                ]
        , body =
            Http.jsonBody (encodeJwtBody body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }