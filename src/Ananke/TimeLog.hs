{-# LANGUAGE DeriveDataTypeable #-}

module Ananke.TimeLog 
  ( LogEntry(..)
  , LogInterval(..)
  , LogEvent(..)
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
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Control.Applicative
import Control.Exception.Base

data LogEvent = StartWork | StopWork deriving (Show, Eq)

data LogEventParseError = LogEventParseError String deriving (Show, Typeable)
instance Exception LogEventParseError where

instance FromField LogEvent where
  fromField f m = let fromText "start_work" = return StartWork
                      fromText "stop_work"  = return StopWork
                      fromText a = conversionError $ LogEventParseError $ "unrecognized log event type " ++ a
                  in fromField f m >>= fromText

data LogEntry = LogEntry { btcAddr :: BtcAddr
                         , logTime :: UTCTime
                         , event :: LogEvent 
                         } deriving (Show, Eq)

instance FromRow LogEntry where
  fromRow = LogEntry <$> field <*> field <*> field 

data LogInterval = LogInterval { intervalBtcAddr :: BtcAddr
                               , workInterval :: Interval
                               } deriving (Show, Eq) 

type WorkIndex = Map BtcAddr ([LogEntry], [LogInterval])
type Payouts = Map BtcAddr Rational

sumLogIntervals :: [LogInterval] -> Rational
sumLogIntervals ivals = F.foldl' (+) (fromInteger 0) $ fmap (toRational . ilen . workInterval) ivals

payouts :: WorkIndex -> Payouts
payouts widx = let addIntervalDiff total ivals = (\dt -> (dt + total, dt)) $ sumLogIntervals ivals 
                   (totalTime, keyTimes) = M.mapAccum addIntervalDiff (fromInteger 0) $ M.map snd widx
               in M.map (\kt -> kt / totalTime) keyTimes

intervals :: Foldable f => f LogEntry -> WorkIndex
intervals = F.foldl' appendLogEntry M.empty 

appendLogEntry :: WorkIndex -> LogEntry -> WorkIndex
appendLogEntry workIndex entry = let acc = reduceToIntervals $ pushEntry entry workIndex 
                                 in insert (btcAddr entry) acc workIndex

pushEntry :: LogEntry -> WorkIndex -> ([LogEntry], [LogInterval])
pushEntry entry = first (entry :) . findWithDefault ([], []) (btcAddr entry) 

reduceToIntervals :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduceToIntervals ((LogEntry addr end StopWork) : (LogEntry _ start StartWork) : xs, intervals) = (xs, (LogInterval addr (interval start end)) : intervals)
reduceToIntervals misaligned = misaligned

