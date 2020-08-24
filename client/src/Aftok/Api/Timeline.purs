module Aftok.Api.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (withExceptT, runExceptT)

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.Either (Either, note)
import Data.Foldable (class Foldable, foldMapDefaultR, intercalate, foldr, foldl)
import Data.JSDate as JD
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)
import Data.UUID as UUID
import Type.Proxy (Proxy(..))
-- import Text.Format as F -- (format, zeroFill, width)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Affjax (get, post)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

import Data.Argonaut.Encode (encodeJson)

import Aftok.Project (ProjectId(..), pidStr)
import Aftok.Types (APIError, JsonCompose, decompose, parseDatedResponse)

data TimelineError
  = LogFailure (APIError)
  | Unexpected String

instance showTimelineError :: Show TimelineError where
  show = case _ of
    LogFailure e -> show e
    Unexpected t -> t

data Event' i
  = StartEvent i
  | StopEvent i

type Event = Event' Instant

derive instance eventFunctor :: Functor Event'

instance eventFoldable :: Foldable Event' where
  foldr f b = case _ of
    StartEvent a -> f a b
    StopEvent a -> f a b
  foldl f b = case _ of
    StartEvent a -> f b a
    StopEvent a -> f b a
  foldMap = foldMapDefaultR

instance eventTraversable :: Traversable Event' where
  traverse f = case _ of
    StartEvent a -> StartEvent <$> f a 
    StopEvent a -> StopEvent <$> f a 
  sequence = traverse identity

instance decodeJsonEvent :: DecodeJson (Event' String) where
  decodeJson json = do
    obj <- decodeJson json
    event <- obj .: "event"
    start' <- traverse (_ .: "eventTime") =<< event .:? "start"
    stop' <-  traverse (_ .: "eventTime") =<< event .:? "stop"
    note "Only 'stop' and 'start' events are supported." $ (StartEvent <$> start') <|> (StopEvent <$> stop')

newtype Interval' i = Interval
  { start :: i
  , end :: i
  }

derive instance intervalEq :: (Eq i) => Eq (Interval' i)
derive instance intervalNewtype :: Newtype (Interval' i) _

instance showInterval :: Show i => Show (Interval' i) where
  show (Interval i) = "Interval {start: " <> show i.start <> ", end: " <> show i.end <> "}"

type Interval = Interval' Instant

derive instance intervalFunctor :: Functor Interval'

instance intervalFoldable :: Foldable Interval' where
  foldr f b (Interval i) = f i.start (f i.end b) 
  foldl f b (Interval i) = f (f b i.start) i.end 
  foldMap = foldMapDefaultR

instance intervalTraversable :: Traversable Interval' where
  traverse f (Interval i) = interval <$> f i.start <*> f i.end 
  sequence = traverse identity

instance decodeJsonInterval :: DecodeJson (Interval' String) where
  decodeJson json = do
    obj <- decodeJson json
    interval <$> obj .: "start" <*> obj .: "end"

interval :: forall i. i -> i -> Interval' i
interval s e = Interval { start: s, end: e }

start :: forall i. Interval' i -> i
start (Interval i) = i.start

end :: forall i. Interval' i -> i
end (Interval i) = i.end

data TimeSpan' t
  = Before t
  | During (Interval' t)
  | After t

type TimeSpan = TimeSpan' DateTime

derive instance timeSpanFunctor :: Functor TimeSpan'
instance timeSpanFoldable :: Foldable TimeSpan' where
  foldr f b = case _ of
    Before a -> f a b
    During x -> foldr f b x
    After  a -> f a b
  foldl f b = case _ of
    Before a -> f b a
    During x -> foldl f b x
    After  a -> f b a
  foldMap = foldMapDefaultR

instance timeSpanTraversable :: Traversable TimeSpan' where
  traverse f = case _ of
    Before a -> Before <$> f a
    During x -> During <$> traverse f x
    After  a -> After <$> f a
  sequence = traverse identity

apiLogStart :: ProjectId -> Aff (Either TimelineError Instant)
apiLogStart (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logStart") requestBody
  liftEffect <<< runExceptT $ do
    event <- withExceptT LogFailure $ parseDatedResponse response
    case event of 
      StartEvent t -> pure t
      StopEvent  _ -> throwError <<< Unexpected $ "Expected start event, got stop."

apiLogEnd :: ProjectId -> Aff (Either TimelineError Instant)
apiLogEnd (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logEnd") requestBody
  liftEffect <<< runExceptT $ do
    event <- withExceptT LogFailure $ parseDatedResponse response
    case event of
      StartEvent _ -> throwError <<< Unexpected $ "Expected stop event, got start."
      StopEvent  t -> pure t

newtype ListIntervalsResponse a = ListIntervalsResponse
  { workIndex :: Array ({ intervals :: Array a }) 
  }

derive instance listIntervalsResponseNewtype :: Newtype (ListIntervalsResponse a) _
derive instance listIntervalsResponseFunctor :: Functor ListIntervalsResponse

instance listIntervalsResponseFoldable :: Foldable ListIntervalsResponse where
  foldr f b (ListIntervalsResponse r) = foldr f b (r.workIndex >>= _.intervals)
  foldl f b (ListIntervalsResponse r) = foldl f b (r.workIndex >>= _.intervals)
  foldMap = foldMapDefaultR

instance listIntervalsResponseTraversable :: Traversable ListIntervalsResponse where
  traverse f (ListIntervalsResponse r) = 
    let traverseCreditRow r' = ({ intervals: _ }) <$> traverse f r'.intervals
     in (ListIntervalsResponse <<< ({ workIndex: _ })) <$> traverse traverseCreditRow r.workIndex
  sequence = traverse identity

instance listIntervalsResponseDecodeJson :: DecodeJson a => DecodeJson (ListIntervalsResponse a) where
  decodeJson = map ListIntervalsResponse <<< decodeJson

_ListIntervalsResponse :: Proxy (JsonCompose ListIntervalsResponse Interval' String)
_ListIntervalsResponse = Proxy

apiListIntervals :: ProjectId -> TimeSpan -> Aff (Either TimelineError (Array Interval))
apiListIntervals pid ts = do
  ts' <- liftEffect $ traverse (JD.toISOString <<< JD.fromDateTime) ts
  let queryElements = case ts' of
        Before t -> ["before=" <> t]
        During (Interval x) -> ["after=" <> x.start, "before=" <> x.end]
        After  t -> ["after=" <> t]
  response <- get RF.json ("/api/user/projects/" <> pidStr pid <> "/workIndex?" <> intercalate "&" queryElements)
  liftEffect 
    <<< runExceptT 
    <<< map (\(ListIntervalsResponse r) -> r.workIndex >>= (_.intervals))
    <<< map decompose
    <<< withExceptT LogFailure 
      $ parseDatedResponse response

