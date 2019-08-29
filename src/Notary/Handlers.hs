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

import qualified Data.ByteString.Base64 as B64
import Crypto.Random.Types

import System.Log.FastLogger                      ( pushLogStrLn, toLogStr )

salt :: SaltRequest -> AppM Salt
salt body = do
  (salt :: ByteString) <- liftIO $ getRandomBytes 12
  pure $ Salt $ toS $ B64.encode salt

signup :: SignupRequest -> AppM NoContent
signup body = do
  getTime <- asks getTime
  let t = getTime
      jwt = sbjwt body
  -- verifyJWT "" t 
  pushLogEntry $ "let's do some logging! jwt: " <> jwt
  pure NoContent
  where
    err :: ApiError -> Handler NoContent
    err (Error msg) = throwError $ err503 { errBody = toS msg }