{-# LANGUAGE TemplateHaskell #-}

module CoinberryApi.Domain 
    ( Currency(..)
    )
    where

import CoinberryApi.Prelude
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Currency = Currency
    { name :: Text
    , unit :: Text
    }

$(deriveJSON defaultOptions ''Currency)
