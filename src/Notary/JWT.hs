module Notary.JWT where

import Notary.Prelude

import Crypto.JWT
import Data.Time.Clock (UTCTime)
import Data.String (String, fromString)
import Control.Lens
import Data.Aeson.Lens 

verifyJWT :: MonadIO m => String -> UTCTime -> JWK -> LByteString -> m (Either JWTError ClaimsSet)
verifyJWT audience time jwk jwt = runExceptT $ do
  jwt' <- decodeCompact jwt
  claims <- verifyClaimsAt config jwk time jwt'
  if isNothing $ claims ^. claimAud then
    throwError JWTNotInAudience
  else
    pure claims
  where 
    config = defaultJWTValidationSettings (== fromString audience)

kid :: JWK -> Maybe Text 
kid key = 
  key ^. jwkKid

address :: ClaimsSet -> Maybe Text 
address claims = 
  case claims ^. claimSub of
    Nothing -> Nothing
    Just strOU ->  Just $ strOU ^. string
