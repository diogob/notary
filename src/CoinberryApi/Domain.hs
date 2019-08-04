{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CoinberryApi.Domain 
    ( Currency(..)
    , Currencies(..)
    , JwtBody(..)
    , UIMessage(..)
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

data JwtBody = JwtBody
    { jwt :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''JwtBody)

data UIMessage = UIMessage
    { content :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''UIMessage)