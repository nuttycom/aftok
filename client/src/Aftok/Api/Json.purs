module Aftok.Api.Json where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
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

parseJsonDate :: Json -> ExceptT String Effect DateTime
parseJsonDate json = do
  str <- except $ decodeJson json
  parseDate str

parseDate :: String -> ExceptT String Effect DateTime
parseDate str = do
  jsDate <- lift $ JD.parse str
  except
    $ note ("Unable to convert date " <> show jsDate <> " to a valid DateTime value.")
        (JD.toDateTime jsDate)

decodeDatedJson :: forall t. Traversable t => DecodeJson (t String) => Json -> ExceptT String Effect (t DateTime)
decodeDatedJson json = do
  decoded <- except $ decodeJson json
  traverse parseDate decoded

parseDatedResponse ::
  forall t.
  Traversable t =>
  DecodeJson (t String) =>
  Either AJAX.Error (Response Json) ->
  ExceptT APIError Effect (t Instant)
parseDatedResponse = case _ of
  Left err -> throwError $ Error { status: Nothing, message: printError err }
  Right r -> case r.status of
    StatusCode 403 -> throwError $ Forbidden
    StatusCode 200 -> withExceptT (ParseFailure r.body) $ map fromDateTime <$> decodeDatedJson r.body
    other -> throwError $ Error { status: Just other, message: r.statusText }
