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
import Data.Foldable as F
import Data.Map.Strict as M
import Data.Time.Clock
import Data.Typeable.Internal
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Control.Applicative
import Control.Arrow
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

instance Ord LogEntry where
  compare a b = compare (logTime a) (logTime b)

instance FromRow LogEntry where
  fromRow = LogEntry <$> field <*> field <*> field 

data LogInterval = LogInterval { intervalBtcAddr :: BtcAddr
                               , workInterval :: Interval
                               } deriving (Show, Eq) 

type WorkIndex = Map BtcAddr ([LogEntry], [LogInterval])

payouts :: [LogEntry] -> Map BtcAddr Rational
payouts = undefined

intervals :: Foldable f => f LogEntry -> WorkIndex
intervals = F.foldl' appendLogEntry M.empty 

appendLogEntry :: WorkIndex -> LogEntry -> WorkIndex
appendLogEntry workIndex entry = let acc = reduce $ pushEntry entry workIndex 
                                 in insert (btcAddr entry) acc workIndex

pushEntry :: LogEntry -> WorkIndex -> ([LogEntry], [LogInterval])
pushEntry entry idx = consLeft entry $ findWithDefault ([], []) (btcAddr entry) idx where 
                      consLeft a (ex, ix) = (a : ex, ix)

reduce :: ([LogEntry], [LogInterval]) -> ([LogEntry], [LogInterval])
reduce ((LogEntry addr end StopWork) : (LogEntry _ start StartWork) : xs, intervals) = (xs, (LogInterval addr (interval start end)) : intervals)
reduce other = other
