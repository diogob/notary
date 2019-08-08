module Notary.JWT (verifyJWT, verifySelfSignedJWT) where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)

verifyJWT :: StringOrURI -> UTCTime -> JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $
  verifyClaimsAt config jwk time jwt
  where 
    config = defaultJWTValidationSettings (== audience)

verifySelfSignedJWT :: StringOrURI -> UTCTime -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifySelfSignedJWT audience time jwt = runExceptT $
  verifyClaimsAt config jwk time jwt
  where 
    config = defaultJWTValidationSettings (== audience)
    jwk = undefined :: JWK
