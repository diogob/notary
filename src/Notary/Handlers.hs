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
      maybeKid = kid jwk
      audience = publicUri cfg
  when (isNothing maybeKid) $ throwError $ err400 { errBody = "Missing kid" }
  claimsOrError <- verifyJWT (toS audience) t jwk (toS jwt)
  case claimsOrError of
    Right c ->
      -- pushLogEntry $ "JWT Claims: " <> show c
      pure NoContent
    Left e -> err $ Error $ "Invalid JWT: " <> show e
  where
    err :: ApiError -> AppM NoContent
    err (Error msg) = throwError $ err400 { errBody = toS msg }