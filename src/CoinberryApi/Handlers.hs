module CoinberryApi.Handlers 
    ( listCurrencies
    ) where

import CoinberryApi.Prelude
import CoinberryApi.Domain
import CoinberryApi.Database (Pool, UsageError, currencies)
import Data.Vector
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant

listCurrencies :: Pool -> Handler (Vector Currency)
listCurrencies pool = liftIO (currencies pool) >>= either err return
  where
    err :: UsageError -> Handler (Vector Currency)
    err msg = throwError $ err503 { errBody = "Error" }