{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module CoinberryApi.Domain 
    ( Signup
    , JwtBody(..)
    , UIMessage(..)
    )
    where

import CoinberryApi.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson (ToJSON)
import Data.Vector

data Signup = Signup
    { jwk :: Text
    , address :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''Signup)

data JwtBody = JwtBody
    { jwt :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''JwtBody)

data UIMessage = UIMessage
    { content :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''UIMessage)