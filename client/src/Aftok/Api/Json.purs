module Aftok.Api.Json where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Functor.Compose (Compose(..))
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.JSDate as JD
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, over)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Affjax as AJAX
import Affjax (Response, printError)
import Affjax.StatusCode (StatusCode(..))
import Aftok.Api.Types (APIError(..))

newtype JsonCompose f g a
  = JsonCompose (Compose f g a)

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

parseJsonDate :: Json -> ExceptT JsonDecodeError Effect DateTime
parseJsonDate json = do
  str <- except $ decodeJson json
  (withExceptT TypeMismatch $ parseDate str)

parseDate :: String -> ExceptT String Effect DateTime
parseDate str = do
  jsDate <- lift $ JD.parse str
  except
    $ note ("Unable to convert date " <> show jsDate <> " to a valid DateTime value.")
        (JD.toDateTime jsDate)

type Decode a
  = Json -> Either JsonDecodeError a

parseResponse ::
  forall a.
  Decode a -> 
  Either AJAX.Error (Response Json) ->
  Aff (Either APIError a)
parseResponse decode response = 
  runExceptT $ case response of 
    Left err -> throwError $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> throwError $ Forbidden
      StatusCode 200 -> withExceptT ParseFailure <<< except $ decode r.body
      other -> throwError $ Error { status: Just other, message: r.statusText }

decodeDatedJson :: 
  forall t. 
  Traversable t => 
  Decode (t String) -> 
  Json -> 
  ExceptT JsonDecodeError Effect (t DateTime)
decodeDatedJson decode json = do
  decoded <- except $ decode json
  (withExceptT TypeMismatch $ traverse parseDate decoded)

parseDatedResponse ::
  forall t.
  Traversable t =>
  Decode (t String) ->
  Either AJAX.Error (Response Json) ->
  ExceptT APIError Effect (t Instant)
parseDatedResponse decode = case _ of
  Left err -> throwError $ Error { status: Nothing, message: printError err }
  Right r -> case r.status of
    StatusCode 403 -> throwError $ Forbidden
    StatusCode 200 -> withExceptT ParseFailure $ map fromDateTime <$> decodeDatedJson decode r.body
    other -> throwError $ Error { status: Just other, message: r.statusText }

parseDatedResponseMay ::
  forall t.
  Traversable t =>
  Decode (t String) ->
  Either AJAX.Error (Response Json) ->
  ExceptT APIError Effect (Maybe (t Instant))
parseDatedResponseMay decode = case _ of
  Left err -> throwError $ Error { status: Nothing, message: printError err }
  Right r -> case r.status of
    StatusCode 403 -> throwError $ Forbidden
    StatusCode 404 -> pure Nothing
    StatusCode 200 ->
      map Just
      <<< withExceptT ParseFailure
      <<< map (map fromDateTime)
      $ decodeDatedJson decode r.body
    other -> 
      throwError $ Error { status: Just other, message: r.statusText }
