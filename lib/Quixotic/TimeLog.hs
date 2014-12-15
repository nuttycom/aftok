{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.TimeLog 
  ( LogEntry(..)
  , btcAddr, event
  , LogInterval(..)
  , EventType(..)
  , eventName, nameEvent
  , WorkEvent(..)
  , eventType, eventTime
  , WorkIndex
  , workIndex
  , Depreciation(..)
  , Months(Months)
  , Payouts
  , payouts
  , linearDepreciation
  ) where

import ClassyPrelude

import Control.Lens
import Data.Aeson
import Data.Foldable as F
import Data.Map.Strict as MS
import Data.Ratio()
import Data.Time.Clock

import Quixotic
import Quixotic.Interval

data EventType = StartWork | StopWork deriving (Show, Eq)

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
  } deriving (Show, Eq)
makeLenses ''WorkEvent

instance FromJSON WorkEvent where
  parseJSON (Object jv) = 
    WorkEvent <$> (jv .: "type" >>= nameEvent) <*> jv .: "timestamp"     

  parseJSON _ = mzero

data LogEntry = LogEntry 
  { _btcAddr :: BtcAddr
  , _event :: WorkEvent 
  } deriving (Show, Eq)
makeLenses ''LogEntry

instance FromJSON LogEntry where
  parseJSON (Object jv) = 
    LogEntry <$> jv .: "btcAddr" <*> jv .: "workEvent"

  parseJSON _ = mzero

data LogInterval = LogInterval 
  { intervalBtcAddr :: BtcAddr
  , workInterval :: Interval
  } deriving (Show, Eq) 

type WorkIndex = Map BtcAddr ([WorkEvent], [Interval])
type Payouts = Map BtcAddr Rational
type NDT = NominalDiffTime

{-|
  The depreciation function should return a value between 0 and 1;
  this result is multiplied by the length of an interval of work to determine
  the depreciated value of the work.
-}
newtype Depreciation = Depreciation { depf :: NDT -> Rational } 

{-|
  Payouts are determined by computing a depreciated duration value for
  each work interval. This function computes the percentage of the total
  work allocated to each address.
-}
payouts :: Depreciation -> UTCTime -> WorkIndex -> Payouts
payouts dep ptime widx = 
  let addIntervalDiff :: (Functor f, Foldable f) => NDT -> f Interval -> (NDT, NDT)
      addIntervalDiff total ivals = (\dt -> (dt + total, dt)) $ workCredit dep ptime ivals 
      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff (fromInteger 0) $ MS.map snd widx
  in  MS.map (\kt -> toRational $ kt / totalTime) keyTimes

{-|
  Given a depreciation function, the "current" time, and a foldable functor of log intervals,
  produce the total, depreciated length of work to be credited to an address.
-}
workCredit :: (Functor f, Foldable f) => Depreciation -> UTCTime -> f Interval -> NDT
workCredit dep ptime ivals = F.foldl' (+) (fromInteger 0) $ fmap (depreciateInterval dep ptime) ivals

{-|
  Compute the depreciated difftime for a single Interval value.
-}
depreciateInterval :: Depreciation -> UTCTime -> Interval -> NDT
depreciateInterval dep ptime ival = 
  let depreciation :: Rational
      depreciation = depf dep $ diffUTCTime ptime (end $ ival)
  in  fromRational $ depreciation * (toRational . ilen $ ival)

workIndex :: Foldable f => f LogEntry -> WorkIndex
workIndex logEntries = 
  let logSum :: RawIndex
      logSum = F.foldl' appendLogEntry MS.empty logEntries
  in  MS.map (bimap (fmap (^. event)) (fmap workInterval)) $ logSum

type RawIndex = Map BtcAddr ([LogEntry], [LogInterval])

appendLogEntry :: RawIndex -> LogEntry -> RawIndex
appendLogEntry idx entry = 
  let acc = reduceToIntervals $ pushEntry entry idx 
  in  insert (entry ^. btcAddr) acc idx

pushEntry :: LogEntry -> RawIndex -> ([LogEntry], [LogInterval])
pushEntry entry = first (entry :) . MS.findWithDefault ([], []) (entry ^. btcAddr) 

reduceToIntervals :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduceToIntervals ((LogEntry addr (WorkEvent StopWork end')) : (LogEntry _ (WorkEvent StartWork start')) : xs, acc) = 
  (xs, (LogInterval addr (interval start' end')) : acc) 
reduceToIntervals misaligned = 
  misaligned

newtype Months = Months Integer 

monthsLength :: Months -> NominalDiffTime
monthsLength (Months i) = fromInteger $ 60 * 60 * 24 * 30 * i

linearDepreciation :: Months -> Months -> Depreciation
linearDepreciation undepPeriod depPeriod = 
  let maxDepreciable :: NominalDiffTime
      maxDepreciable = monthsLength undepPeriod + monthsLength depPeriod 

      zeroTime :: NominalDiffTime
      zeroTime = fromInteger 0

      depf' :: NominalDiffTime -> Rational
      depf' dt = if dt < monthsLength undepPeriod 
        then 1
        else toRational (max zeroTime (maxDepreciable - dt)) / toRational maxDepreciable
  in Depreciation depf'

-- data LogEventParseError = LogEventParseError String deriving (Show, Typeable)
-- instance Exception LogEventParseError where

-- instance FromField WorkEvent where
--   fromField f m = let fromText "start_work" = return StartWork
--                       fromText "stop_work"  = return StopWork
--                       fromText a = conversionError $ LogEventParseError $ "unrecognized log event type " ++ a
--                   in fromField f m >>= fromText

-- instance FromRow LogEntry where
--   fromRow = LogEntry <$> field <*> field <*> field 

