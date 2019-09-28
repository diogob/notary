{-# LANGUAGE ScopedTypeVariables #-}

module Notary.Handlers 
    ( salt
    , signup
    ) where

import Notary.Prelude
import Notary.Domain
import Notary.AppM
import Notary.JWT
import qualified Notary.Database as DB
import Data.Vector
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant
import Data.String (String)

import qualified Data.ByteString.Base64 as B64
import Crypto.Random.Types

import System.Log.FastLogger                      ( pushLogStrLn, toLogStr )

salt :: SaltRequest -> AppM Salt
salt body = do
  pool <- asks getPool
  saltOrError <- DB.salt pool $ sraddress body
  case saltOrError of 
    Right salt -> pure $ Salt $ toS $ B64.encode salt
    Left e -> err e
  where
    err :: ApiError -> AppM Salt
    err (Error msg) = throwError $ err503 { errBody = toS msg }

signup :: SignupRequest -> AppM NoContent
signup body = do
  getTime <- asks getTime
  cfg <- asks config
  t <- liftIO getTime
  let jwt = sbjwt body
      jwk = sbpublicKey body
      audience = publicUri cfg
  claimsOrError <- verifyJWT (toS audience) t jwk undefined
  case claimsOrError of
    Right c -> do
      pushLogEntry $ "let's do some logging! jwt: " <> jwt
      pure NoContent
    Left _ -> err $ Error "Invalid JWT"
  where
    err :: ApiError -> AppM NoContent
    err (Error msg) = throwError $ err503 { errBody = toS msg }