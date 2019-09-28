module Notary.JWT where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)
import Data.String (String, fromString)

verifyJWT :: MonadIO m => String -> UTCTime -> JWK -> LByteString -> m (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $ do
  jwt' <- decodeCompact jwt
  verifyClaimsAt config jwk time jwt'
  where 
    config = defaultJWTValidationSettings (== fromString audience)
