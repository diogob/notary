module Generated.Coinberry API exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


postSignup : JwtBody -> Http.Request (UIMessage)
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
            Http.jsonBody (encodeJwtBody body)
        , expect =
            Http.expectJson decodeUIMessage
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postConfirm : JwtBody -> Http.Request (UIMessage)
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
            Http.expectJson decodeUIMessage
        , timeout =
            Nothing
        , withCredentials =
            False
        }

patchEmail : JwtBody -> Http.Request (UIMessage)
patchEmail body =
    Http.request
        { method =
            "PATCH"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "email"
                ]
        , body =
            Http.jsonBody (encodeJwtBody body)
        , expect =
            Http.expectJson decodeUIMessage
        , timeout =
            Nothing
        , withCredentials =
            False
        }

patchSignature : JwtBody -> Http.Request (UIMessage)
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
            Http.expectJson decodeUIMessage
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSignature : JwtBody -> Http.Request (UIMessage)
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
            Http.expectJson decodeUIMessage
        , timeout =
            Nothing
        , withCredentials =
            False
        }