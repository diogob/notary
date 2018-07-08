{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module CoinberryApi.Api
    ( server
    , api

    -- ** re-exports
    , serve
    ) where

import CoinberryApi.Database (Pool)
import CoinberryApi.Prelude
import CoinberryApi.Domain
import CoinberryApi.Handlers

import Data.Vector

import Servant

type API = "currencies" :> Get '[JSON] (Vector Currency)

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = listCurrencies
