{-# LANGUAGE QuasiQuotes #-}
module DatabaseSpec (spec) where

import Protolude hiding (get)

import Notary
import Notary.Database
import Test.Hspec
import Data.Aeson.QQ

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
    describe "signup" $
      it "should be successful" $ \p -> do
        token <- signup p "foo@bar.com" [aesonQQ| { } |]
        token `shouldSatisfy` isRight
