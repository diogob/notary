module Notary.Database 
    ( salt

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
import Data.Either.Combinators (mapLeft)
import Data.Vector hiding (sequence)

salt :: MonadIO m => Pool -> Text -> m (Either ApiError ByteString)
salt pool address = liftIO mapError
  where
    mapError = mapLeft (\_ -> Error "Database Error") <$> (use pool $ statement address selectSalt)
    selectSalt :: Statement Text ByteString
    selectSalt = Statement sql encoder decoder True
    sql = "SELECT notary.signup($1)"
    encoder = HE.param (HE.nonNullable HE.text)
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.bytea))