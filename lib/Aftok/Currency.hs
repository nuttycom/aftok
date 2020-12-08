module Aftok.Currency where

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)

data Network a
  = Network
      { addressFromJSON :: Parser a,
        addressToJSON :: a -> Value
      }
