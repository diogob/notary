module CoinberryApi.Handlers 
    ( signup
    ) where

import CoinberryApi.Prelude
import CoinberryApi.Domain
import CoinberryApi.AppM
import qualified CoinberryApi.Database as DB
import Data.Vector
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant

import System.Log.FastLogger                      ( pushLogStrLn, toLogStr )

signup :: JwtBody -> AppM NoContent
signup jwt = do
  logset <- asks getLogger

  let logMsg = LogMessage { message = "let's do some logging!"
  , level = "info"
  , lversion = "1.1.1"
  , lenvironment = "development"
  }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
  pure NoContent
  where
    err :: ApiError -> Handler NoContent
    err (Error msg) = throwError $ err503 { errBody = toS msg }