module Aftok.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Control.Monad.Trans.Class (lift)

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Functor.Compose (Compose(..))
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.JSDate as JD
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, over)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now, nowDateTime)
import Affjax as AJAX
import Affjax (Response, printError)
import Affjax.StatusCode (StatusCode(..))

import Effect.Class.Console as C
import Web.Event.Event as WE

type System m =
  { log :: String -> m Unit
  , error :: String -> m Unit
  , now :: m Instant
  , nowDateTime :: m DateTime
  , preventDefault :: WE.Event -> m Unit
  , dateFFI :: DateFFI m
  }

liveSystem :: System Aff
liveSystem = 
  { log: liftEffect <<< C.log
  , error: liftEffect <<< C.error
  , now: liftEffect now
  , nowDateTime: liftEffect nowDateTime
  , preventDefault: liftEffect <<< WE.preventDefault
  , dateFFI: hoistDateFFI liftEffect jsDateFFI
  }

type DateFFI m =
  { midnightLocal :: Instant -> m (Maybe (Tuple Date Instant))
  }

jsDateFFI :: DateFFI Effect
jsDateFFI = 
  { midnightLocal
  }

midnightLocal :: Instant -> Effect (Maybe (Tuple Date Instant))
midnightLocal i = do
  let jsDate = JD.fromInstant i
  year  <- JD.getFullYear jsDate
  month <- JD.getMonth jsDate
  day   <- JD.getDate jsDate
  jsMidnight <- midnightLocalJS year month day
  let date = JD.toDate jsMidnight
  pure $ Tuple <$> date <*> JD.toInstant jsMidnight

midnightLocalJS :: Number -> Number -> Number -> Effect JD.JSDate
midnightLocalJS year month day = JD.jsdateLocal
  { year
  , month
  , day
  , hour: 0.0
  , minute: 0.0
  , second: 0.0
  , millisecond: 0.0
  }

hoistDateFFI :: forall m n. (forall a. m a -> n a) -> DateFFI m -> DateFFI n
hoistDateFFI nt ffi = 
    { midnightLocal: \i -> nt (ffi.midnightLocal i)
    }

data APIError 
  = Forbidden
  | ParseFailure Json String
  | Error { status :: Maybe StatusCode, message :: String }

instance showAPIError :: Show APIError where
  show = case _ of
    Forbidden -> "Forbidden"
    ParseFailure js e -> "ParseFailure (" <> show (stringify js) <> ") " <> show e 
    Error r -> "Error { status: " <> show r.status <> ", message: " <> r.message <> "}"

newtype JsonCompose f g a = JsonCompose (Compose f g a)
derive instance jsonComposeNewtype :: Newtype (JsonCompose f g a) _

instance jsonComposeFunctor :: (Functor f, Functor g) => Functor (JsonCompose f g) where
  map f = over JsonCompose (map f)

instance jsonComposeFoldable :: (Foldable f, Foldable g) => Foldable (JsonCompose f g) where
  foldr f b = foldr f b <<< unwrap
  foldl f b = foldl f b <<< unwrap
  foldMap f = foldMap f <<< unwrap

instance jsonComposeTraversable :: (Traversable f, Traversable g) => Traversable (JsonCompose f g) where
  traverse f = map JsonCompose <<< traverse f <<< unwrap
  sequence = traverse identity

instance jsonComposeDecodeJson :: (DecodeJson (f (g a))) => DecodeJson (JsonCompose f g a) where
  decodeJson json = JsonCompose <<< Compose <$> decodeJson json

decompose :: forall f g a. JsonCompose f g a -> f (g a)
decompose (JsonCompose (Compose fga)) = fga

parseJsonDate :: Json -> ExceptT String Effect DateTime
parseJsonDate json = do
  str    <- except $ decodeJson json
  parseDate str

parseDate :: String -> ExceptT String Effect DateTime
parseDate str = do
  jsDate <- lift $ JD.parse str
  except $ note ("Unable to convert date " <> show jsDate <> " to a valid DateTime value.") 
                (JD.toDateTime jsDate)

decodeDatedJson :: forall t. Traversable t => DecodeJson (t String) => Json -> ExceptT String Effect (t DateTime)
decodeDatedJson json = do
  decoded <- except $ decodeJson json
  traverse parseDate decoded

parseDatedResponse 
  :: forall t
  .  Traversable t
  => DecodeJson (t String) 
  => Either AJAX.Error (Response Json) 
  -> ExceptT APIError Effect (t Instant)
parseDatedResponse = case _ of
  Left err -> 
    throwError $ Error { status: Nothing, message: printError err }
  Right r -> case r.status of
    StatusCode 403 -> 
      throwError $ Forbidden
    StatusCode 200 -> 
      withExceptT (ParseFailure r.body) $ map fromDateTime <$> decodeDatedJson r.body
    other -> 
      throwError $ Error { status: Just other, message: r.statusText }

