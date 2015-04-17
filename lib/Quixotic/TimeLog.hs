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
  , WorkIndex
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
import Data.Aeson as A
import Data.Aeson.Types
import Data.Foldable as F
import Data.Map.Strict as MS
import Data.Ratio()
import Data.Time.Clock

import Quixotic
import Quixotic.Json
import Quixotic.Interval

data EventType = StartWork | StopWork deriving (Show, Eq, Typeable)

eventName :: EventType -> Text
eventName StartWork = "start"
eventName StopWork  = "stop"

nameEvent :: MonadPlus m => Text -> m EventType
nameEvent "start" = return StartWork
nameEvent "stop"  = return StopWork
nameEvent _       = mzero

data WorkEvent = WorkEvent 
  { _eventType :: EventType
  , _eventTime :: UTCTime
  -- Permit the inclusion of arbitrary JSON data that may be refactored into
  -- proper typed fields in the future. 
  , _eventMeta :: Maybe A.Value
  } deriving (Show, Eq)
makeLenses ''WorkEvent

data LogEntry = LogEntry 
  { _btcAddr :: BtcAddr
  , _event :: WorkEvent 
  } deriving (Show, Eq)
makeLenses ''LogEntry

newtype EventId = EventId Int64 deriving (Show, Eq)
makePrisms ''EventId

newtype ModTime = ModTime UTCTime
makePrisms ''ModTime

data LogModification = TimeChange ModTime UTCTime
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

type WorkIndex = Map BtcAddr [Interval]
type RawIndex = Map BtcAddr ([LogEntry], [LogInterval])
type NDT = NominalDiffTime

workIndexJSON :: WorkIndex -> Value
workIndexJSON widx = toJSON $ (fmap intervalJSON) <$> (MS.mapKeysWith (++) (^._BtcAddr) widx)

eventIdJSON :: EventId -> Value
eventIdJSON (EventId eid) = toJSON eid

{-|
  The depreciation function should return a value between 0 and 1;
  this result is multiplied by the length of an interval of work to determine
  the depreciated value of the work.
-}
type DepF = UTCTime -> Interval -> NDT 

{-|
  Payouts are determined by computing a depreciated duration value for
  each work interval. This function computes the percentage of the total
  work allocated to each address.
-}
payouts :: DepF -> UTCTime -> WorkIndex -> Payouts
payouts dep ptime widx = 
  let addIntervalDiff :: (Functor f, Foldable f) => NDT -> f Interval -> (NDT, NDT)
      addIntervalDiff total ivals = (\dt -> (dt + total, dt)) $ workCredit dep ptime ivals 
      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff (fromInteger 0) $ widx
  in  Payouts $ fmap (\kt -> toRational $ kt / totalTime) keyTimes

{-|
  Given a depreciation function, the "current" time, and a foldable functor of log intervals,
  produce the total, depreciated length of work to be credited to an address.
-}
workCredit :: (Functor f, Foldable f) => DepF -> UTCTime -> f Interval -> NDT
workCredit depf ptime ivals = 
  F.foldl' (+) (fromInteger 0) $ fmap (depf ptime) ivals

workIndex :: Foldable f => f LogEntry -> WorkIndex
workIndex logEntries = 
  let logSum :: RawIndex
      logSum = F.foldl' appendLogEntry MS.empty logEntries
  in  fmap (fmap workInterval . snd) $ logSum

appendLogEntry :: RawIndex -> LogEntry -> RawIndex
appendLogEntry idx entry = 
  let acc = reduceToIntervals $ pushEntry entry idx 
  in  insert (entry ^. btcAddr) acc idx

pushEntry :: LogEntry -> RawIndex -> ([LogEntry], [LogInterval])
pushEntry entry = first (entry :) . MS.findWithDefault ([], []) (entry ^. btcAddr) 

reduceToIntervals :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduceToIntervals ((LogEntry addr (WorkEvent StopWork end' _)) : (LogEntry _ (WorkEvent StartWork start' _)) : xs, acc) = 
  (xs, (LogInterval addr (interval start' end')) : acc) 
reduceToIntervals misaligned = 
  misaligned

newtype Months = Months Integer 

monthsLength :: Months -> NominalDiffTime
monthsLength (Months i) = fromInteger $ 60 * 60 * 24 * 30 * i

linearDepreciation :: Months -> Months -> DepF
linearDepreciation undepPeriod depPeriod = \ptime ival -> 
  let maxDepreciable :: NominalDiffTime
      maxDepreciable = monthsLength undepPeriod + monthsLength depPeriod 

      zeroTime :: NominalDiffTime
      zeroTime = fromInteger 0

      depPct :: NominalDiffTime -> Rational
      depPct dt = 
        if dt < monthsLength undepPeriod then 1
        else toRational (max zeroTime (maxDepreciable - dt)) / toRational maxDepreciable

      depreciation = depPct $ diffUTCTime ptime (ival ^. end)
  in  fromRational $ depreciation * (toRational $ ilen ival) 
