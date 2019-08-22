module Notary.Handlers 
    ( signup
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

import System.Log.FastLogger                      ( pushLogStrLn, toLogStr )

signup :: SignupBody -> AppM NoContent
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