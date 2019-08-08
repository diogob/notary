module Notary.JWT (verifyJWT) where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)

verifyJWT :: StringOrURI -> UTCTime -> JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== audience)
  verifyClaimsAt config jwk time jwt