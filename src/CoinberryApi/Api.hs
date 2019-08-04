{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CoinberryApi.Api
    ( server
    , api

    -- ** re-exports
    , serve
    , Proxy
    , API
    ) where

import CoinberryApi.Database (Pool, currencies)
import CoinberryApi.Prelude
import CoinberryApi.Domain
import CoinberryApi.Handlers

import Data.Vector
import Data.Swagger
import Servant.Swagger
import Lens.Micro
import Elm (ElmType)

import Servant

{-
POST signup
POST confirm
PATCH email
PATCH signature
-}

type PublicApi = 
                 "signup" :> ReqBody '[JSON] JwtBody :> Post '[JSON] UIMessage
            :<|> "confirm" :> ReqBody '[JSON] JwtBody :> Post '[JSON] UIMessage
            :<|> "email" :> ReqBody '[JSON] JwtBody :> Patch '[JSON] UIMessage
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Patch '[JSON] UIMessage
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Delete '[JSON] UIMessage

type AdminApi = "verify" :> Get '[JSON] Currencies

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = PublicApi

instance ToSchema JwtBody
instance ElmType JwtBody
instance ToSchema UIMessage
instance ElmType UIMessage


api :: Proxy PublicApi
api = Proxy

coinberrySwagger :: Swagger
coinberrySwagger = toSwagger api
  & info.title   .~ "Coinberry API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: Pool -> Server API
server pool = panic "need to implement api"
