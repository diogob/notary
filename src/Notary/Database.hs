module Notary.Database 
    ( salt
    , signup
    , jwkForKid
    -- re-exports
    , Pool
    , UsageError
    , acquire
    , release
    ) where


import Notary.Prelude
import Notary.Domain
import Hasql.Statement (Statement (..))
import Hasql.Session (Session, statement)
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Hasql.Pool (Pool, UsageError, acquire, release, use)
import Data.Either.Combinators (mapLeft, mapBoth)
import Data.Vector hiding (sequence)
import qualified Data.Aeson as JSON
import Data.Functor.Contravariant ((>$<))

salt :: MonadIO m => Pool -> Text -> m (Either ApiError ByteString)
salt pool address = liftIO mapError
  where
    mapError = mapLeft (\_ -> Error "Database Error") <$> use pool (statement address selectSalt)
    selectSalt :: Statement Text ByteString
    selectSalt = Statement sql encoder decoder True
    sql = "SELECT notary.salt($1)"
    encoder = HE.param (HE.nonNullable HE.text)
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.bytea))

signup :: MonadIO m => Pool -> Text -> JSON.Value -> m (Either ApiError Text)
signup pool address key = liftIO mapError
  where
    mapError = mapLeft (\_ -> Error "Database Error") <$> use pool (statement (address, key) selectSignup)
    selectSignup :: Statement (Text, JSON.Value) Text
    selectSignup = Statement sql encoder decoder True
    sql = "SELECT notary.signup($1, $2)"
    encoder = 
      (fst >$< HE.param (HE.nonNullable HE.text)) <> 
      (snd >$< HE.param (HE.nonNullable HE.jsonb))
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.text))

jwkForKid :: MonadIO m => Pool -> Text -> m (Either ApiError JSON.Value)
jwkForKid pool kid = liftIO mapError
  where
    mapError = mapLeft (\_ -> Error "Database Error (could be null return in jwk_for_kid)") <$> use pool (statement kid selectPublicKey)
    selectPublicKey :: Statement Text JSON.Value
    selectPublicKey = Statement sql encoder decoder True
    sql = "SELECT notary.jwk_for_kid($1)"
    encoder = HE.param (HE.nonNullable HE.text)
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.jsonb))
