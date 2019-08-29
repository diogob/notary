{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Notary.Domain 
    ( Signup
    , JwtBody(..)
    , SignupRequest(..)
    , UIMessage(..)
    , Salt(..)
    , SaltRequest(..)
    )
    where

import Notary.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson (ToJSON)
import Data.Vector hiding (drop)

data Signup = Signup
    { sjwt :: Text
    , saddress :: Text
    , spublicKey :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''Signup)

newtype JwtBody = JwtBody
    { jbjwt :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''JwtBody)

data SignupRequest = SignupRequest
    { sbjwt :: Text
    , sbpublicKey :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''SignupRequest)

data SaltRequest = SaltRequest
    { sraddress :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''SaltRequest)

newtype Salt = Salt
    { unsalt :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''Salt)


newtype UIMessage = UIMessage
    { ucontent :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''UIMessage)