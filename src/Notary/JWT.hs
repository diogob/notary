module Notary.JWT where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)

verifyJWT :: MonadIO m => StringOrURI -> UTCTime -> JWK -> SignedJWT -> m (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $
  verifyClaimsAt config jwk time jwt
  where 
    config = defaultJWTValidationSettings (== audience)
