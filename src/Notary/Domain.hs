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
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson (ToJSON)
import Data.Vector hiding (drop)

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

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''SignupBody)

newtype UIMessage = UIMessage
    { ucontent :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''UIMessage)