{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Quixotic.TimeLog 
  ( LogEntry(..)
  , btcAddr, event
  , LogInterval(..)
  , EventType(..)
  , eventName, nameEvent
  , WorkEvent(..)
  , eventType, eventTime, eventMeta
  , WorkIndex(WorkIndex), _WorkIndex
  , workIndex, workIndexJSON
  , DepF
  , EventId(EventId), _EventId, eventIdJSON
  , ModTime(ModTime), _ModTime
  , LogModification(..)
  , Months(Months)
  , Payouts(..), _Payouts
  , payouts
  , linearDepreciation
  ) where

import ClassyPrelude

import Control.Lens
import Data.AdditiveGroup
import Data.Aeson as A
import Data.Aeson.Types
import Data.AffineSpace
import Data.Heap as H
import Data.List.NonEmpty as L
import Data.Foldable as F
import Data.Map.Strict as MS
import Data.Ratio()
import Data.Thyme.Clock as C
import Data.VectorSpace

import Quixotic
import Quixotic.Json
import Quixotic.Interval

data EventType = StartWork | StopWork deriving (Show, Eq, Typeable)

instance Ord EventType where
  compare StartWork StopWork = GT
  compare StopWork StartWork = LT
  compare _ _ = EQ

eventName :: EventType -> Text
eventName StartWork = "start"
eventName StopWork  = "stop"

nameEvent :: MonadPlus m => Text -> m EventType
nameEvent "start" = return StartWork
nameEvent "stop"  = return StopWork
nameEvent _       = mzero

data WorkEvent = WorkEvent 
  { _eventType :: EventType
  , _eventTime :: C.UTCTime
  -- Permit the inclusion of arbitrary JSON data that may be refactored into
  -- proper typed fields in the future. 
  , _eventMeta :: Maybe A.Value
  } deriving (Show, Eq)
makeLenses ''WorkEvent

instance Ord WorkEvent where
  compare a b = 
    let cv x = (x ^. eventTime, x ^. eventType)
    in compare (cv a) (cv b)

data LogEntry = LogEntry 
  { _btcAddr :: BtcAddr
  , _event :: WorkEvent 
  } deriving (Show, Eq)
makeLenses ''LogEntry

instance Ord LogEntry where
  compare a b = 
    let ordElems e = (e ^. (event.eventTime), e ^. (event.eventType), e ^. btcAddr)
    in  ordElems a `compare` ordElems b

newtype EventId = EventId Int64 deriving (Show, Eq)
makePrisms ''EventId

newtype ModTime = ModTime C.UTCTime
makePrisms ''ModTime

data LogModification = TimeChange ModTime C.UTCTime
                     | AddressChange ModTime BtcAddr
                     | MetadataChange ModTime A.Value

data LogInterval = LogInterval 
  { intervalBtcAddr :: BtcAddr
  , workInterval :: Interval
  } deriving (Show, Eq) 

newtype Payouts = Payouts (Map BtcAddr Rational)
makePrisms ''Payouts

payoutsJSON :: Payouts -> Value
payoutsJSON (Payouts m) = toJSON $ MS.mapKeys (^. _BtcAddr) m

parsePayoutsJSON :: Value -> Parser Payouts
parsePayoutsJSON v = 
  Payouts . MS.mapKeys BtcAddr <$> parseJSON v 

instance A.ToJSON Payouts where
  toJSON = versioned (Version 1 0 0) . payoutsJSON

instance A.FromJSON Payouts where
  parseJSON v = let parsePayouts (Version 1 0 0) = parsePayoutsJSON 
                    parsePayouts v' = \_ -> fail . show $ printVersion v'
                in  unversion parsePayouts $ v

newtype WorkIndex = WorkIndex (Map BtcAddr (NonEmpty Interval)) deriving (Show, Eq)
makePrisms ''WorkIndex

type RawIndex = Map BtcAddr [Either WorkEvent Interval]
type NDT = C.NominalDiffTime

workIndexJSON :: WorkIndex -> Value
workIndexJSON (WorkIndex widx) = 
  toJSON $ (L.toList . fmap intervalJSON) <$> (MS.mapKeysMonotonic (^._BtcAddr) widx)

eventIdJSON :: EventId -> Value
eventIdJSON (EventId eid) = toJSON eid

{-|
 - The depreciation function should return a value between 0 and 1;
 - this result is multiplied by the length of an interval of work to determine
 - the depreciated value of the work.
 -}
type DepF = C.UTCTime -> Interval -> NDT 

{-|
 - Given a depreciation function, the "current" time, and a foldable functor of log intervals,
 - produce the total, depreciated length of work to be credited to an address.
 -}
workCredit :: (Functor f, Foldable f) => DepF -> C.UTCTime -> f Interval -> NDT
workCredit depf ptime ivals = getSum $ F.foldMap (Sum . depf ptime) ivals

{-|
 - Payouts are determined by computing a depreciated duration value for
 - each work interval. This function computes the percentage of the total
 - work allocated to each address.
 -}
payouts :: DepF -> C.UTCTime -> WorkIndex -> Payouts
payouts dep ptime (WorkIndex widx) = 
  let addIntervalDiff :: (Functor f, Foldable f) => NDT -> f Interval -> (NDT, NDT)
      addIntervalDiff total ivals = (^+^ total) &&& id $ workCredit dep ptime ivals 

      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff zeroV $ widx

  in  Payouts $ fmap ((/ toSeconds totalTime) . toSeconds) keyTimes

workIndex :: Foldable f => f LogEntry -> WorkIndex
workIndex logEntries = 
  let sortedEntries = F.foldr H.insert H.empty $ logEntries
      rawIndex = F.foldl' appendLogEntry MS.empty $ sortedEntries

      accum k l m = case nonEmpty (rights l) of 
        Just l' -> MS.insert k l' m
        Nothing -> m

  in  WorkIndex $ MS.foldrWithKey accum MS.empty rawIndex

appendLogEntry :: RawIndex -> LogEntry -> RawIndex
appendLogEntry idx (LogEntry k ev) = 
  let combine (WorkEvent StartWork t _) (WorkEvent StopWork t' _)  | t' > t = Right $ Interval t t'
      combine (e1 @ (WorkEvent StartWork _ _)) (e2 @ (WorkEvent StartWork _ _)) = Left $ max e1 e2
      combine (e1 @ (WorkEvent StopWork  _ _)) (e2 @ (WorkEvent StopWork  _ _)) = Left $ min e1 e2
      combine _ e2 = Left e2

      ivals = case MS.lookup k idx of
        Just (Left ev' : xs) -> combine ev' ev : xs
        Just xs -> Left ev : xs
        Nothing -> Left ev : []

  in  MS.insert k ivals idx

newtype Months = Months Integer 

{-|
 - A very simple linear function for calculating depreciation.
 -
 -}
linearDepreciation :: Months -> -- ^ The number of initial months during which no depreciation occurs
                      Months -> -- ^ The number of months over which each logged interval will be depreciated
                      DepF
linearDepreciation undepPeriod depPeriod = 
  let monthsLength :: Months -> NDT
      monthsLength (Months i) = fromSeconds $ 60 * 60 * 24 * 30 * i

      maxDepreciable :: NDT
      maxDepreciable = monthsLength undepPeriod ^+^ monthsLength depPeriod 

      depPct :: NDT -> Rational
      depPct dt = 
        if dt < monthsLength undepPeriod then 1
        else toSeconds (max zeroV (maxDepreciable ^-^ dt)) / toSeconds maxDepreciable

  in  \ptime ival -> 
    let depreciation = depPct $ ptime .-. (ival ^. end)
    in  depreciation *^ (ilen ival)
