module Aftok.Api.Timeline where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (withExceptT, runExceptT)
import Data.Array (head)
import Data.Argonaut.Core (Json)
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
import Foreign.Object (Object)
-- import Type.Proxy (Proxy(..))
-- import Text.Format as F -- (format, zeroFill, width)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Affjax (get, post)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Argonaut.Encode (encodeJson)
import Aftok.Types (ProjectId(..), pidStr)
import Aftok.Api.Types (APIError)
import Aftok.Api.Json (decompose, parseDatedResponse)

data TimelineError
  = LogFailure (APIError)
  | Unexpected String

instance showTimelineError :: Show TimelineError where
  show = case _ of
    LogFailure e -> show e
    Unexpected t -> t

data Event t
  = StartEvent t
  | StopEvent t

eventTime :: forall i. Event i -> i
eventTime = case _ of
  StartEvent t -> t
  StopEvent t -> t

instance showEvent :: (Show i) => Show (Event i) where
  show = case _ of
    StartEvent t -> "Start " <> show t
    StopEvent t -> "Stop " <> show t

derive instance eventFunctor :: Functor Event

instance eventFoldable :: Foldable Event where
  foldr f b = case _ of
    StartEvent a -> f a b
    StopEvent a -> f a b
  foldl f b = case _ of
    StartEvent a -> f b a
    StopEvent a -> f b a
  foldMap = foldMapDefaultR

instance eventTraversable :: Traversable Event where
  traverse f = case _ of
    StartEvent a -> StartEvent <$> f a
    StopEvent a -> StopEvent <$> f a
  sequence = traverse identity

parseEventFields :: Object Json -> Either String (Event String)
parseEventFields obj = do
  ev <- obj .: "event"
  start' <- traverse (_ .: "eventTime") =<< ev .:? "start"
  stop' <- traverse (_ .: "eventTime") =<< ev .:? "stop"
  note "Only 'stop' and 'start' events are supported."
    $ (StartEvent <$> start')
    <|> (StopEvent <$> stop')

instance eventDecodeJSON :: DecodeJson (Event String) where
  decodeJson = parseEventFields <=< decodeJson

newtype KeyedEvent i
  = KeyedEvent
  { eventId :: String
  , event :: Event i
  }

keyedEvent :: forall i. String -> Event i -> KeyedEvent i
keyedEvent eid ev = KeyedEvent { eventId: eid, event: ev }

eventId :: forall i. KeyedEvent i -> String
eventId (KeyedEvent xs) = xs.eventId

event :: forall i. KeyedEvent i -> Event i
event (KeyedEvent xs) = xs.event

derive instance keyedEventFunctor :: Functor KeyedEvent

instance keyedEventFoldable :: Foldable KeyedEvent where
  foldr f b = foldr f b <<< event
  foldl f b = foldl f b <<< event
  foldMap = foldMapDefaultR

instance keyedEventTraversable :: Traversable KeyedEvent where
  traverse f (KeyedEvent xs) = (\ev -> KeyedEvent { eventId: xs.eventId, event: ev }) <$> traverse f xs.event
  sequence = traverse identity

instance keyedEventDecodeJson :: DecodeJson (KeyedEvent String) where
  decodeJson json = do
    obj <- decodeJson json
    keyedEvent <$> obj .: "eventId" <*> parseEventFields obj

newtype Interval i
  = Interval
  { start :: i
  , end :: i
  }

derive instance intervalEq :: (Eq i) => Eq (Interval i)

derive instance intervalNewtype :: Newtype (Interval i) _

instance showInterval :: Show i => Show (Interval i) where
  show (Interval i) = "Interval {start: " <> show i.start <> ", end: " <> show i.end <> "}"

type TimeInterval
  = Interval Instant

derive instance intervalFunctor :: Functor Interval

instance intervalFoldable :: Foldable Interval where
  foldr f b (Interval i) = f i.start (f i.end b)
  foldl f b (Interval i) = f (f b i.start) i.end
  foldMap = foldMapDefaultR

instance intervalTraversable :: Traversable Interval where
  traverse f (Interval i) = interval <$> f i.start <*> f i.end
  sequence = traverse identity

instance intervalDecodeJSON :: DecodeJson i => DecodeJson (Interval i) where
  decodeJson json = do
    obj <- decodeJson json
    interval <$> obj .: "start" <*> obj .: "end"

interval :: forall i. i -> i -> Interval i
interval s e = Interval { start: s, end: e }

start :: forall i. Interval i -> i
start (Interval i) = i.start

end :: forall i. Interval i -> i
end (Interval i) = i.end

data TimeSpan' t
  = Before t
  | During (Interval t)
  | After t

type TimeSpan
  = TimeSpan' DateTime

derive instance timeSpanFunctor :: Functor TimeSpan'

instance timeSpanFoldable :: Foldable TimeSpan' where
  foldr f b = case _ of
    Before a -> f a b
    During x -> foldr f b x
    After a -> f a b
  foldl f b = case _ of
    Before a -> f b a
    During x -> foldl f b x
    After a -> f b a
  foldMap = foldMapDefaultR

instance timeSpanTraversable :: Traversable TimeSpan' where
  traverse f = case _ of
    Before a -> Before <$> f a
    During x -> During <$> traverse f x
    After a -> After <$> f a
  sequence = traverse identity

apiLogStart :: ProjectId -> Aff (Either TimelineError (KeyedEvent Instant))
apiLogStart (ProjectId pid) = do
  let
    requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logStart") requestBody
  liftEffect <<< runExceptT
    $ do
        kev <- withExceptT LogFailure $ parseDatedResponse decodeJson response
        case event kev of
          StartEvent _ -> pure kev
          StopEvent _ -> throwError <<< Unexpected $ "Expected start event, got stop."

apiLogEnd :: ProjectId -> Aff (Either TimelineError (KeyedEvent Instant))
apiLogEnd (ProjectId pid) = do
  let
    requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logEnd") requestBody
  liftEffect <<< runExceptT
    $ do
        kev <- withExceptT LogFailure $ parseDatedResponse decodeJson response
        case event kev of
          StartEvent _ -> throwError <<< Unexpected $ "Expected stop event, got start."
          StopEvent _ -> pure kev

newtype ListIntervalsResponse a
  = ListIntervalsResponse
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
    let
      traverseCreditRow r' = ({ intervals: _ }) <$> traverse f r'.intervals
    in
      (ListIntervalsResponse <<< ({ workIndex: _ })) <$> traverse traverseCreditRow r.workIndex
  sequence = traverse identity

instance listIntervalsResponseDecodeJson :: DecodeJson a => DecodeJson (ListIntervalsResponse a) where
  decodeJson = map ListIntervalsResponse <<< decodeJson

apiListIntervals :: ProjectId -> TimeSpan -> Aff (Either TimelineError (Array (Interval (KeyedEvent Instant))))
apiListIntervals pid ts = do
  ts' <- liftEffect $ traverse (JD.toISOString <<< JD.fromDateTime) ts
  let
    queryElements = case ts' of
      Before t -> [ "before=" <> t, "limit=100" ]
      During (Interval x) -> [ "after=" <> x.start, "before=" <> x.end, "limit=100" ]
      After t -> [ "after=" <> t, "limit=100" ]
  response <- get RF.json ("/api/user/projects/" <> pidStr pid <> "/workIndex?" <> intercalate "&" queryElements)
  liftEffect
    <<< runExceptT
    <<< map (\(ListIntervalsResponse r) -> r.workIndex >>= (_.intervals))
    <<< map (map decompose <<< decompose)
    <<< withExceptT LogFailure
    $ parseDatedResponse decodeJson response

apiLatestEvent :: ProjectId -> Aff (Either TimelineError (Maybe (KeyedEvent Instant)))
apiLatestEvent pid = do
  response <- get RF.json ("/api/user/projects/" <> pidStr pid <> "/events")
  liftEffect
    <<< runExceptT
    <<< map head
    <<< map decompose
    <<< withExceptT LogFailure
    $ parseDatedResponse decodeJson response
