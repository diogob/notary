module CoinberryApi.Handlers 
    ( listCurrencies
    ) where

import CoinberryApi.Prelude
import CoinberryApi.Domain
import Data.Vector
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant

listCurrencies :: Handler (Either ApiError Currencies)  -> Handler Currencies
listCurrencies c = c >>= either err return
  where
    err :: ApiError -> Handler Currencies
    err (Error msg) = throwError $ err503 { errBody = toS msg }