module Notary.Prelude
    ( module Exports
    , ApiError (..)
    ) where

import Protolude as Exports

data ApiError = Error Text