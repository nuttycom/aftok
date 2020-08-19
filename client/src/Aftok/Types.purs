module Aftok.Types where

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Affjax.StatusCode (StatusCode)

data APIError 
  = Forbidden
  | ParseFailure Json String
  | Error { status :: Maybe StatusCode, message :: String }


