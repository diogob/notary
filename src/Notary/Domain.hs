{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Notary.Domain 
    ( Signup
    , JwtBody(..)
    , SignupBody(..)
    , UIMessage(..)
    )
    where

import Notary.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson (ToJSON)
import Data.Vector

data Signup = Signup
    { sjwt :: Text
    , saddress :: Text
    , spublic_key :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''Signup)

newtype JwtBody = JwtBody
    { jbjwt :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''JwtBody)

data SignupBody = SignupBody
    { sbjwt :: Text
    , sbpublic_key :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''SignupBody)

newtype UIMessage = UIMessage
    { ucontent :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''UIMessage)