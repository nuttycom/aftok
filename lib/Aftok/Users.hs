module Aftok.Users
  ( RegisterOps(..)
  , RegisterError(..)
  )
  where

import           Aftok.Types (Email(..))
import           Aftok.Currency.Zcash (ZAddr, ZAddrError)

data RegisterError
  = ZAddrParseError ZAddrError

data RegisterOps m = RegisterOps
  { parseZAddr :: Text -> m (Either ZAddrError ZAddr)
  , sendConfirmationEmail :: Email -> m ()
  }
