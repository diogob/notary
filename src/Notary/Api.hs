{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}

module Notary.Api
    ( server
    , api
    , mkApp

    -- ** re-exports
    , serve
    , Proxy
    , API
    ) where

import Notary.Database (Pool)
import Notary.Prelude
import Notary.Domain
import Notary.Handlers
import Notary.AppM

import Data.Vector
import Data.Swagger
import Servant.Swagger
import Lens.Micro
import Elm (ElmType)
import Crypto.JWT (JWK)

import Servant

type PublicApi = 
                 "salt" :> ReqBody '[JSON] SaltRequest :> Post '[JSON] Salt
            :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] NoContent
            :<|> "confirm" :> ReqBody '[JSON] JwtBody :> Post '[JSON] NoContent
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Patch '[JSON] NoContent
            :<|> "signature" :> ReqBody '[JSON] JwtBody :> Delete '[JSON] NoContent

type AdminApi = "verify" :> Get '[JSON] NoContent

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = PublicApi
instance ToSchema JWK where
  declareNamedSchema _ = do
    doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
    return $ NamedSchema (Just "JWK") $ mempty
      & required .~ [ "key" ]
instance ToSchema JwtBody
instance ToSchema SignupRequest
instance ToSchema UIMessage
instance ToSchema Salt
instance ToSchema SaltRequest

api :: Proxy PublicApi
api = Proxy

coinberrySwagger :: Swagger
coinberrySwagger = toSwagger api
  & info.title   .~ "Coinberry API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: ServerT API AppM
server = salt :<|> signup :<|> confirm :<|> undefined

nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT x s

mkApp :: AppCtx -> Application
mkApp s = serve api $ hoistServer api (nt s) server
