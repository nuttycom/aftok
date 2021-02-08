module Aftok.Types where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..))
import Data.Date (Date, year, month, day)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.Either (note)
import Data.Enum (fromEnum)
import Data.JSDate as JD
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, toString, parseUUID)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now, nowDateTime)
import Effect.Class.Console as C
import Type.Row as Row
import Web.Event.Event as WE
import Web.HTML (HTMLElement)
import Routing.Hash as RH
import Halogen as H
import Halogen.HTML as HH
import Halogen.Portal (portalAff)
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.HTML.QRious as QRious

type System m
  = { log :: String -> m Unit
    , error :: String -> m Unit
    , now :: m Instant
    , getHash :: m String
    , setHash :: String -> m Unit
    , nowDateTime :: m DateTime
    , preventDefault :: WE.Event -> m Unit
    , dateFFI :: DateFFI m
    , portal :: 
      forall query action input output slots label slot _1.
        Row.Cons label (H.Slot query output slot) _1 slots =>
        IsSymbol label =>
        Ord slot =>
        Monad m =>
        SProxy label ->
        slot ->
        H.Component HH.HTML query input output m ->
        input ->
        Maybe HTMLElement ->
        (output -> Maybe action) ->
        H.ComponentHTML action slots m
    , toggleModal :: String -> ModalFFI.Toggle -> m Unit
    , renderQR :: QRious.QROpts -> m String
    }

liveSystem :: System Aff
liveSystem =
  { log: liftEffect <<< C.log
  , error: liftEffect <<< C.error
  , now: liftEffect now
  , getHash: liftEffect RH.getHash
  , setHash: liftEffect <<< RH.setHash
  , nowDateTime: liftEffect nowDateTime
  , preventDefault: liftEffect <<< WE.preventDefault
  , dateFFI: hoistDateFFI liftEffect jsDateFFI
  , portal: portalAff
  , toggleModal: \i t -> liftEffect (ModalFFI.toggleModal i t)
  , renderQR: \opts -> liftEffect (QRious.renderQR opts)
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

instance userIdDecodeJson :: DecodeJson UserId where
  decodeJson json = do
    uuidStr <- decodeJson json
    UserId <$> note (TypeMismatch "Failed to decode user UUID") (parseUUID uuidStr)

newtype ProjectId
  = ProjectId UUID

derive instance projectIdEq :: Eq ProjectId

derive instance projectIdOrd :: Ord ProjectId

derive instance projectIdNewtype :: Newtype ProjectId _

instance projectIdDecodeJson :: DecodeJson ProjectId where
  decodeJson json = do
    uuidStr <- decodeJson json
    ProjectId <$> note (TypeMismatch "Failed to decode project UUID") (parseUUID uuidStr)

pidStr :: ProjectId -> String
pidStr (ProjectId uuid) = toString uuid

dateStr :: Date -> String
dateStr d =
  (show <<< fromEnum $ year d) <> "-"
    <> (show <<< fromEnum $ month d)
    <> "-"
    <> (show <<< fromEnum $ day d)
