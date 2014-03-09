{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Ananke.TimeLog 
  ( LogEntry(..)
  , LogInterval(..)
  , WorkEvent(..)
  , WorkIndex
  , payouts
  , intervals
  ) where

import Ananke
import Ananke.Interval
import Data.Bifunctor
import Data.Function
import Data.Foldable as F
import Data.Map.Strict as M
import Data.Time.Clock
import Data.Typeable.Internal
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple.FromField
import Control.Applicative
import Control.Exception.Base

data WorkEvent = StartWork { logTime :: UTCTime }
              | StopWork  { logTime :: UTCTime } deriving (Show, Eq)

data LogEventParseError = LogEventParseError String deriving (Show, Typeable)
instance Exception LogEventParseError where

-- instance FromField WorkEvent where
--   fromField f m = let fromText "start_work" = return StartWork
--                       fromText "stop_work"  = return StopWork
--                       fromText a = conversionError $ LogEventParseError $ "unrecognized log event type " ++ a
--                   in fromField f m >>= fromText

data LogEntry = LogEntry { btcAddr :: BtcAddr
                         , event :: WorkEvent 
                         } deriving (Show, Eq)

-- instance FromRow LogEntry where
--   fromRow = LogEntry <$> field <*> field <*> field 

data LogInterval = LogInterval { intervalBtcAddr :: BtcAddr
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
      (totalTime, keyTimes) = M.mapAccum addIntervalDiff (fromInteger 0) $ M.map snd widx
  in M.map (\kt -> toRational $ kt / totalTime) keyTimes

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

intervals :: Foldable f => f LogEntry -> WorkIndex
intervals logEntries = M.map (bimap (fmap event) (fmap workInterval)) $ F.foldl' appendLogEntry M.empty logEntries

type RawIndex = Map BtcAddr ([LogEntry], [LogInterval])

appendLogEntry :: RawIndex -> LogEntry -> RawIndex
appendLogEntry workIndex entry = 
  let acc = reduceToIntervals $ pushEntry entry workIndex 
  in insert (btcAddr entry) acc workIndex

pushEntry :: LogEntry -> RawIndex -> ([LogEntry], [LogInterval])
pushEntry entry = first (entry :) . findWithDefault ([], []) (btcAddr entry) 

reduceToIntervals :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduceToIntervals ((LogEntry addr (StopWork end)) : (LogEntry _ (StartWork start)) : xs, intervals) = 
  (xs, (LogInterval addr (interval start end)) : intervals)
reduceToIntervals misaligned = misaligned

