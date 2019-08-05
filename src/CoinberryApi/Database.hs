module CoinberryApi.Database 
    ( signup

    -- re-exports
    , Pool
    , UsageError
    , acquire
    , release
    ) where


import CoinberryApi.Prelude
import CoinberryApi.Domain
import Hasql.Statement (Statement (..))
import Hasql.Session (Session, statement)
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Hasql.Pool (Pool, UsageError, acquire, release, use)
import Data.Either.Combinators (mapLeft)
import Data.Vector hiding (sequence)

signup :: MonadIO m => Pool -> JwtBody -> m (Either ApiError ()) 
signup p jwt = undefined
  where
    mapError = mapLeft (\_ -> Error "Database Error") <$> use p currenciesQuery
    currenciesQuery = statement () currenciesStatement
    currenciesStatement =
        Statement sql encoder decoder True
    sql =
        "SELECT name, unit FROM public.currencies"
    encoder = HE.unit
    decoder =
        HD.rowList $ Currency <$> HD.column HD.text <*> HD.column HD.text