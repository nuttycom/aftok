module Aftok.Types where

import Prelude
import Data.Date (Date, year, month, day)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.Enum (fromEnum)
import Data.JSDate as JD
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, toString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now, nowDateTime)
import Effect.Class.Console as C
import Web.Event.Event as WE
import Routing.Hash as H

type System m
  = { log :: String -> m Unit
    , error :: String -> m Unit
    , now :: m Instant
    , getHash :: m String
    , setHash :: String -> m Unit
    , nowDateTime :: m DateTime
    , preventDefault :: WE.Event -> m Unit
    , dateFFI :: DateFFI m
    }

liveSystem :: System Aff
liveSystem =
  { log: liftEffect <<< C.log
  , error: liftEffect <<< C.error
  , now: liftEffect now
  , getHash: liftEffect H.getHash
  , setHash: liftEffect <<< H.setHash
  , nowDateTime: liftEffect nowDateTime
  , preventDefault: liftEffect <<< WE.preventDefault
  , dateFFI: hoistDateFFI liftEffect jsDateFFI
  }

type DateFFI m
  = { midnightLocal :: Instant -> m (Maybe (Tuple Date Instant))
    }

jsDateFFI :: DateFFI Effect
jsDateFFI =
  { midnightLocal
  }

midnightLocal :: Instant -> Effect (Maybe (Tuple Date Instant))
midnightLocal i = do
  let
    jsDate = JD.fromInstant i
  year <- JD.getFullYear jsDate
  month <- JD.getMonth jsDate
  day <- JD.getDate jsDate
  jsMidnight <- midnightLocalJS year month day
  let
    date = JD.toDate jsMidnight
  pure $ Tuple <$> date <*> JD.toInstant jsMidnight

midnightLocalJS :: Number -> Number -> Number -> Effect JD.JSDate
midnightLocalJS year month day =
  JD.jsdateLocal
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

newtype UserId
  = UserId UUID

derive instance userIdEq :: Eq UserId 
derive instance userIdOrd :: Ord UserId 

derive instance userIdNewtype :: Newtype UserId _

newtype ProjectId
  = ProjectId UUID

derive instance projectIdEq :: Eq ProjectId

derive instance projectIdNewtype :: Newtype ProjectId _

pidStr :: ProjectId -> String
pidStr (ProjectId uuid) = toString uuid

dateStr :: Date -> String
dateStr d =
  (show <<< fromEnum $ year d) <> "-"
    <> (show <<< fromEnum $ month d)
    <> "-"
    <> (show <<< fromEnum $ day d)

