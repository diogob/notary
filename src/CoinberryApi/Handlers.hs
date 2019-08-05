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

signup :: JwtBody -> AppM NoContent
signup jwt = do
  undefined
  where
    err :: ApiError -> Handler NoContent
    err (Error msg) = throwError $ err503 { errBody = toS msg }