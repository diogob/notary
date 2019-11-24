{-# LANGUAGE QuasiQuotes #-}

-- | This module exposes all functions to send HTTP messages
module Notary.HTTPClient 
    ( sendConfirmationToken
    ) where

import Notary.Prelude
import Notary.Domain
import Network.URI
import Network.HTTP.Simple
import Data.Aeson.QQ

sendConfirmationToken :: MonadIO m => URI -> Text -> m ()
sendConfirmationToken uri token = 
    liftIO mRequest >>= void . httpNoBody 
    where
        mRequest = setRequestBodyJSON body <$> initRequest
        body = [aesonQQ| {token: #{token}} |]
        initRequest :: IO Request
        initRequest = parseRequestThrow uriToString'
        uriToString' = "POST " <> uriToString identity uri ""