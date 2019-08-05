{-# LANGUAGE DeriveGeneric #-}

module CoinberryApi.AppM where

import CoinberryApi.Prelude
import CoinberryApi.Database

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson (ToJSON (..), FromJSON, genericToEncoding, encode)
import Data.Time.Clock          (UTCTime, getCurrentTime)
import Control.AutoUpdate       ( defaultUpdateSettings
                                , mkAutoUpdate
                                , updateAction
                                )
import Control.Monad.Time

import System.Log.FastLogger                      ( ToLogStr(..)
                                                  , LoggerSet
                                                  , defaultBufSize
                                                  , newStdoutLoggerSet
                                                  , flushLogStr
                                                  , pushLogStrLn )

import Servant

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx {
    getLogger :: LoggerSet
  , getPool :: Pool
  , getTime :: IO UTCTime
  }

data LogMessage = LogMessage {
    ltime        :: !UTCTime     
  , lmessage     :: !Text
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

mkLogger :: IO LoggerSet
mkLogger = newStdoutLoggerSet defaultBufSize

mkGetTime :: IO (IO UTCTime) 
mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

pushLogEntry :: Text -> AppM ()
pushLogEntry msg = do
  logset <- asks getLogger
  getTime <- asks getTime
  time <- liftIO getTime

  let logMsg = LogMessage { ltime = time
      , lmessage = msg
      , level = "info"
      , lversion = "1.1.1"
      , lenvironment = "development"
      }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
