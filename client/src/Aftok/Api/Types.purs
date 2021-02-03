module Aftok.Api.Types where

import Prelude
import Affjax.StatusCode (StatusCode)
import Data.Argonaut.Core (Json, stringify)
import Data.Maybe (Maybe)

data APIError
  = Forbidden
  | ParseFailure Json String
  | Error { status :: Maybe StatusCode, message :: String }

instance showAPIError :: Show APIError where
  show = case _ of
    Forbidden -> "Forbidden"
    ParseFailure js e -> "ParseFailure (" <> show (stringify js) <> ") " <> show e
    Error r -> "Error { status: " <> show r.status <> ", message: " <> r.message <> "}"

newtype Stored i t = Stored
  { dbid :: i
  , value :: t
  }
