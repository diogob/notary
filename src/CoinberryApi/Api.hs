{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}

module CoinberryApi.Api
    ( server
    , api

    -- ** re-exports
    , serve
    ) where

import CoinberryApi.Prelude

import Hasql.Pool (Pool)

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server _ = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
