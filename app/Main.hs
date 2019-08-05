{-# LANGUAGE DeriveGeneric #-}

module Main where

import CoinberryApi
import           Protolude          hiding (readFile, Text)

import           Network.Wai.Handler.Warp

import           Dhall
import           System.Environment (getArgs)

data Config = Config { db :: Text 
                     , port :: Integer
                     } deriving (Generic, Show)

instance Interpret Config

loadConfig :: IO Config
loadConfig = do
    args <- getArgs
    case args of
        path:_ -> input auto (toS path)
        _      -> die "no config file argument"

main :: IO ()
main = loadConfig >>= startApp

startApp :: Config -> IO ()
startApp conf = do
  putStrLn $ ("Listening on port " :: Text) <> show portNumber
  pool <- acquire (10, 10, toS $ db conf)
  appLogger <- mkLogger
  let ctx = AppCtx appLogger pool
  let runApp = run portNumber $ mkApp $ ctx
  runApp
  where
    portNumber = fromIntegral $ port conf

