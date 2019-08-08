module Main where

import Notary
import           Protolude

import           Network.Wai.Handler.Warp

import Env

data Config = Config { db :: Text 
                     , port :: Integer
                     } deriving (Show)

loadConfig :: IO Config
loadConfig =
  Env.parse (header "Notary") $
    Config <$> var (str <=< nonempty) "NOTARY_DB_URI"  (help "Database to to store signatures")
          <*> var (auto <=< nonempty) "NOTARY_PUBLIC_PORT" (help "Public port for the http server")
   
main :: IO ()
main = loadConfig >>= startApp

startApp :: Config -> IO ()
startApp conf = do
  putStrLn $ ("Listening on port " :: Text) <> show portNumber
  pool <- acquire (10, 10, toS $ db conf)
  appLogger <- mkLogger
  getTime <- mkGetTime
  let ctx = AppCtx appLogger pool getTime
  let runApp = run portNumber $ mkApp ctx
  runApp
  where
    portNumber = fromIntegral $ port conf

