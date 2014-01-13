{-# LANGUAGE DeriveDataTypeable #-}

module Ananke.TimeLog 
  ( LogEntry(..)
  , LogInterval(..)
  , LogEvent(..)
  , payouts
  , intervals
  ) where

import Ananke
import Data.Map
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

data LogInterval = LogInterval { intervalBtcAddr :: BtcAddr
                               , start :: UTCTime
                               , end :: UTCTime
                               } deriving (Show, Eq) 

instance Ord LogEntry where
  compare a b = compare (logTime a) (logTime b)

instance FromRow LogEntry where
  fromRow = LogEntry <$> field <*> field <*> field 


payouts :: [LogEntry] -> Map BtcAddr Rational
payouts = undefined

intervals :: [LogEntry] -> [LogInterval]
intervals e = undefined
