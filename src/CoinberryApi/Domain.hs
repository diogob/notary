{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CoinberryApi.Domain 
    ( Currency(..)
    , Currencies(..)
    )
    where

import CoinberryApi.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson (ToJSON)
import Data.Vector

data Currency = Currency
    { name :: Text
    , unit :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''Currency)

type Currencies = [Currency]

