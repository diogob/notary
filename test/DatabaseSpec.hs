module DatabaseSpec (spec) where

import Protolude hiding (get)

import Notary
import Notary.Database
import Test.Hspec

withDatabaseConnection :: IO Pool
withDatabaseConnection = acquire (10, 10, "postgres://notary_public:test@localhost/notary_test")

spec :: Spec
spec =
  before withDatabaseConnection $
    describe "salt" $
      it "should be successful and idenpotent" $ \p -> do
        previousSalt <- salt p "foo@bar.com"
        previousSalt `shouldSatisfy` isRight
        salt p "foo@bar.com" `shouldReturn` previousSalt
