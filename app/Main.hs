module Main where

import Notary
import           Protolude

import           Network.Wai.Handler.Warp

import Data.String (String)
import Network.URI (URI, parseURI)

import Env

loadConfig :: IO Config
loadConfig =
  Env.parse (header "Notary") $
    Config <$> var (str <=< nonempty) "NOTARY_DB_URI"  (help "Database to to store signatures")
           <*> var (auto <=< nonempty) "NOTARY_PUBLIC_PORT" (help "Public port for the http server")
           <*> var (str <=< nonempty) "NOTARY_PUBLIC_URI" (help "Public URI used for notary public interface")
           <*> var (parseUrl <=< nonempty) "CONFIRMATION_URI" (help "URI used to send confirmation tokens")
   
parseUrl :: String -> Either Env.Error URI
parseUrl url = 
    case parseURI $ toS url of
      Nothing -> panic $ "Invalid URL: " <> toS url
      Just u -> Right u

main :: IO ()
main = loadConfig >>= startApp

startApp :: Config -> IO ()
startApp conf = do
  putStrLn $ ("Listening on port " :: Text) <> show portNumber
  pool <- acquire (10, 10, toS $ db conf)
  appLogger <- mkLogger
  getTime <- mkGetTime
  let ctx = AppCtx conf appLogger pool getTime
  run portNumber $ mkApp ctx
  where
    portNumber = fromIntegral $ port conf

