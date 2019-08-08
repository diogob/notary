module Notary.Prelude
    ( module Exports
    , ApiError (..)
    ) where

import Protolude as Exports

newtype ApiError = Error Text