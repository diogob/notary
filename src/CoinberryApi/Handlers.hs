module CoinberryApi.Handlers 
    ( listCurrencies
    ) where

import CoinberryApi.Prelude
import CoinberryApi.Domain
import Data.Vector
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant

listCurrencies :: Handler (Either ApiError (Vector Currency))  -> Handler (Vector Currency)
listCurrencies c = c >>= either err return
  where
    err :: ApiError -> Handler (Vector Currency)
    err (Error msg) = throwError $ err503 { errBody = toS msg }