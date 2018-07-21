{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

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

import Servant

type CoinberryApi = "currencies" :> Get '[JSON] Currencies

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CoinberryApi

instance ToSchema Currency

api :: Proxy CoinberryApi
api = Proxy

coinberrySwagger :: Swagger
coinberrySwagger = toSwagger api
  & info.title   .~ "Coinberry API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: Pool -> Server API
server pool = return coinberrySwagger :<|> listCurrencies (currencies pool)
