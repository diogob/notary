module CoinberryApi.Database 
    ( currencies

    -- re-exports
    , Pool
    , UsageError
    , acquire
    , release
    ) where


import CoinberryApi.Prelude
import CoinberryApi.Domain
import Hasql.Query (Query, statement)
import Hasql.Session (Session, query)
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Hasql.Pool (Pool, UsageError, acquire, release, use)
import Data.Either.Combinators (mapLeft)
import Data.Vector 

currencies :: Pool -> IO (Either UsageError (Vector Currency)) 
currencies = flip use currenciesQuery
  where
    currenciesQuery = query () currenciesStatement
    currenciesStatement =
        statement sql encoder decoder True
    sql =
        "SELECT name, unit FROM public.currencies"
    encoder = HE.unit
    decoder =
        HD.rowsVector $ Currency <$> HD.value HD.text <*> HD.value HD.text