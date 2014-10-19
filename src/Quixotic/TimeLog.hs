{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Quixotic.TimeLog 
  ( LogEntry(..)
  , LogInterval(..)
  , WorkEvent(..)
  , WorkIndex
  , Depreciation(..)
  , Months(Months)
  , Payouts
  , eventName
  , payouts
  , intervals
  , linearDepreciation
  ) where

import Quixotic
import Quixotic.Interval
import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Function()
import Data.Aeson
import Data.Foldable as F
import Data.Map.Strict as M
import Data.Ratio()
import Data.Time.Clock
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

data WorkEvent = StartWork { logTime :: UTCTime }
               | StopWork  { logTime :: UTCTime } deriving (Show, Eq)

eventName :: WorkEvent -> String
eventName (StartWork _) = "start"
eventName (StopWork  _) = "stop"

instance FromJSON WorkEvent where
  parseJSON (Object jv) = do
    t <- jv .: "type" :: A.Parser T.Text
    case t of
      "start" -> StartWork <$> jv .: "timestamp"     
      "stop"  -> StopWork <$> jv .: "timestamp"
      _ -> mzero
  parseJSON _ = mzero

data LogEntry = LogEntry 
  { btcAddr :: BtcAddr
  , event :: WorkEvent 
  } deriving (Show, Eq)

instance FromJSON LogEntry where
  parseJSON (Object jv) = LogEntry <$>
                          jv .: "btcAddr" <*>
                          jv .: "workEvent"

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
      (totalTime, keyTimes) = M.mapAccum addIntervalDiff (fromInteger 0) $ M.map snd widx
  in  M.map (\kt -> toRational $ kt / totalTime) keyTimes

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
intervals logEntries = 
  let logSum = F.foldl' appendLogEntry M.empty logEntries
  in  M.map (bimap (fmap event) (fmap workInterval)) $ logSum

type RawIndex = Map BtcAddr ([LogEntry], [LogInterval])

appendLogEntry :: RawIndex -> LogEntry -> RawIndex
appendLogEntry workIndex entry = 
  let acc = reduceToIntervals $ pushEntry entry workIndex 
  in  insert (btcAddr entry) acc workIndex

pushEntry :: LogEntry -> RawIndex -> ([LogEntry], [LogInterval])
pushEntry entry = first (entry :) . findWithDefault ([], []) (btcAddr entry) 

reduceToIntervals :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduceToIntervals ((LogEntry addr (StopWork end')) : (LogEntry _ (StartWork start')) : xs, acc) = 
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

