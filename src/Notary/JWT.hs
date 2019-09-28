module Notary.JWT where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)
import Data.String (String, fromString)

verifyJWT :: MonadIO m => String -> UTCTime -> JWK -> SignedJWT -> m (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $
  verifyClaimsAt config jwk time jwt
  where 
    config = defaultJWTValidationSettings (== (fromString audience))
