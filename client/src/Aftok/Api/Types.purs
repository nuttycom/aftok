module Aftok.Api.Types where

import Prelude
import Affjax.StatusCode (StatusCode)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

data APIError
  = Forbidden
  | ParseFailure JsonDecodeError
  | Error { status :: Maybe StatusCode, message :: String }

instance showAPIError :: Show APIError where
  show = case _ of
    Forbidden -> "Forbidden"
    ParseFailure e -> "ParseFailure (" <> show e <> ") " 
    Error r -> "Error { status: " <> show r.status <> ", message: " <> r.message <> "}"

newtype Stored i t = Stored
  { dbid :: i
  , value :: t
  }

data CommsType
  = EmailComms
  | ZcashComms

derive instance commsTypeEq :: Eq CommsType

data CommsAddress
  = EmailCommsAddr String
  | ZcashCommsAddr String

newtype Zip321Request = Zip321Request String

derive instance zip321RequestNewtype :: Newtype Zip321Request _

