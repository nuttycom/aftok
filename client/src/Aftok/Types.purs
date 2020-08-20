module Aftok.Types where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT, except)

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.DateTime (DateTime)
import Data.Either (note)
import Data.JSDate as JSDate
import Data.Maybe (Maybe)

import Effect (Effect)
import Affjax.StatusCode (StatusCode)

data APIError 
  = Forbidden
  | ParseFailure Json String
  | Error { status :: Maybe StatusCode, message :: String }

instance showAPIError :: Show APIError where
  show = case _ of
    Forbidden -> "Forbidden"
    ParseFailure js e -> "ParseFailure (" <> show (stringify js) <> ") " <> show e 
    Error r -> "Error { status: " <> show r.status <> ", message: " <> r.message <> "}"

parseJsonDate :: Json -> ExceptT String Effect DateTime
parseJsonDate json = do
  str    <- except $ decodeJson json
  parseDate str

parseDate :: String -> ExceptT String Effect DateTime
parseDate str = do
  jsDate <- lift $ JSDate.parse str
  except $ note ("Unable to convert date " <> show jsDate <> " to a valid DateTime value.") 
                (JSDate.toDateTime jsDate)

