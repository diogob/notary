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
import Data.Aeson (encode, toJSON)
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
  pool <- asks getPool
  let jwt = sbjwt body
      jwk = sbpublicKey body
      maybeKid = kid jwk
      audience = publicUri cfg
  when (isNothing maybeKid) $ throwError $ err400 { errBody = "Missing kid" } -- optimization only
  claimsOrError <- verifyJWT (toS audience) t jwk (toS jwt)
  case address <$> claimsOrError of
    Right (Just addr) -> do
      tokenOrError <- DB.signup pool addr (toJSON jwk)
      case tokenOrError of
        Right t -> pure NoContent
        Left _ -> err $ Error "Could not create confirmation, possibly kid mismatch"
    Right Nothing -> err $ Error "No sub claim in JWT"
    Left e -> err $ Error $ "Invalid JWT: " <> show e
  where
    err :: ApiError -> AppM NoContent
    err (Error msg) = throwError $ err400 { errBody = toS msg }