{-# LANGUAGE QuasiQuotes #-}
module DatabaseSpec (spec) where

import Protolude hiding (get, hash)

import Notary
import Notary.Database
import Test.Hspec
import Data.Aeson.QQ
import Crypto.Hash
import qualified Data.ByteString.Base64 as B64
import Data.String (String)

hexSha512 :: ByteString -> String
hexSha512 bs = show (hash bs :: Digest SHA512)

withDatabaseConnection :: IO Pool
withDatabaseConnection = acquire (10, 10, "postgres://notary_public:test@localhost/notary_test")

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
