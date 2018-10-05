module Generated.Coinberry API exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


getCurrencies : Http.Request (List (Currency))
getCurrencies =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "currencies"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeCurrency)
        , timeout =
            Nothing
        , withCredentials =
            False
        }