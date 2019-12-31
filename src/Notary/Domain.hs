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
import Crypto.JWT (JWK)

data Signup = Signup
    { sjwt :: Text
    , saddress :: Text
    , spublicKey :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''Signup)

data JwtBody = JwtBody
    { jbjwt :: Text
    , jbkid :: Text
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''JwtBody)

data SignupRequest = SignupRequest
    { sbjwt :: Text
    , sbpublicKey :: JWK
    } deriving (Generic, Typeable)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''SignupRequest)

newtype SaltRequest = SaltRequest
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