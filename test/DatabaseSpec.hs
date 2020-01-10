{-# LANGUAGE QuasiQuotes #-}
module DatabaseSpec (spec) where

import Protolude hiding (hash)

import Notary
import Notary.Database
import Test.Hspec
import Data.Aeson.QQ
import Crypto.Hash
import qualified Data.ByteString.Base64 as B64
import Data.String (String)

import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

hexSha512 :: ByteString -> String
hexSha512 bs = show (hash bs :: Digest SHA512)

doOrDie :: Show e => IO (Either e a) -> IO a
doOrDie acc = do
  res <- acc
  case res of
    Left ex -> panic $ show ex
    Right v -> pure v

withDatabaseConnection :: IO Pool
withDatabaseConnection = do
  c <- doOrDie $ Hasql.acquire "postgres://notary:test@localhost/notary_test"
  doOrDie $ Hasql.run (mapM_ truncateTable tables) c
  acquire (10, 10, "postgres://notary_public:test@localhost/notary_test")
  where
    truncateTable table = Hasql.sql $ "truncate " <> table <> " cascade"
    tables = [ "notary.signups"
             , "notary.confirmations"
             ]

spec :: Spec
spec =
  before withDatabaseConnection $ do
    describe "salt" $
      it "should be successful and idenpotent" $ \p -> do
        previousSalt <- salt p "foo@bar.com"
        previousSalt `shouldSatisfy` isRight
        salt p "foo@bar.com" `shouldReturn` previousSalt
    describe "signup" $ do
      it "should be successful" $ \p -> do
        previousSalt <- salt p "foo@bar.com"
        case previousSalt of 
          Right s -> do
            let kid = hexSha512("foo@bar.com " <> B64.encode s)
            token <- signup p "foo@bar.com" [aesonQQ| { "kid": #{kid} } |]
            token `shouldSatisfy` isRight
          _ -> panic "could not fetch salt"
      it "should fail when kid is undefined" $ \p -> do
        token <- signup p "foo@bar.com" [aesonQQ| { } |]
        token `shouldSatisfy` isLeft
      it "should fail when kid cannot be derived successful" $ \p -> do
        token <- signup p "foo@bar.com" [aesonQQ| { "kid": "not-derived-correctly" } |]
        token `shouldSatisfy` isLeft
    describe "jwkForKid" $ do
      it "should return error when kid is not found" $ \p -> do
        error <- jwkForKid p "not-a-kid"
        error `shouldSatisfy` isLeft
      it "should return public key when kid is found" $ \p -> do
        previousSalt <- salt p "foo@bar.com"
        case previousSalt of 
          Right s -> do
            let kid = hexSha512("foo@bar.com " <> B64.encode s)
            void $ signup p "foo@bar.com" [aesonQQ| { "kid": #{kid} } |]
            key <- jwkForKid p (toS kid)
            key `shouldSatisfy` isRight
          _ -> panic "could not fetch salt"
    describe "confirm" $
      it "should return confirmation date for public key" $ \p -> do
        previousSalt <- salt p "foo@bar.com"
        case previousSalt of 
          Right s -> do
            let kid = hexSha512("foo@bar.com " <> B64.encode s)
            void $ signup p "foo@bar.com" [aesonQQ| { "kid": #{kid} } |]
            confirmedAt <- confirm p [aesonQQ| { "kid": #{kid} } |]
            confirmedAt `shouldSatisfy` isRight
          _ -> panic "could not fetch salt"
