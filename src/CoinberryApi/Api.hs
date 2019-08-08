{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}

module CoinberryApi.Api
    ( server
    , api
    , mkApp

    -- ** re-exports
    , serve
    , Proxy
    , API
    ) where

import CoinberryApi.Database (Pool)
import CoinberryApi.Prelude
import CoinberryApi.Domain
import CoinberryApi.Handlers
import CoinberryApi.AppM

import Data.Vector
import Data.Swagger
import Servant.Swagger
import Lens.Micro
import Elm (ElmType)

import Servant

type PublicApi = 
                 "signup" :> ReqBody '[JSON] JwtBody :> Post '[JSON] NoContent
            :<|> "confirm" :> ReqBody '[JSON] JwtBody :> Post '[JSON] NoContent
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Patch '[JSON] NoContent
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Delete '[JSON] NoContent

type AdminApi = "verify" :> Get '[JSON] NoContent

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

server :: ServerT API AppM
server = signup :<|> undefined

nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT x s

mkApp :: AppCtx -> Application
mkApp s = serve api $ hoistServer api (nt s) server
