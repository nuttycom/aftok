{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Quixotic.Database.SQLite (sqliteADB) where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Database.SQLite
import Quixotic
import qualified Quixotic.Database as D
import Quixotic.TimeLog
import System.Locale (defaultTimeLocale)

sqliteADB :: SQLiteHandle -> IO (D.ADB IO SQLiteHandle)
sqliteADB db = do
  _ <- defineTableOpt db True eventTable
  return $ D.ADB recordEvent readWorkIndex 

recordEvent :: SQLiteHandle -> LogEntry -> IO ()
recordEvent h (LogEntry ba ev) = 
  void $ insertRow h "workEvents" [ ("btcAddr", T.unpack (address ba))
                                  , ("event", eventName ev)
                                  , ("eventTime", formatSqlTime (logTime ev)) ]

readWorkIndex :: SQLiteHandle -> EitherT T.Text IO WorkIndex
readWorkIndex db = do
  let baseResult = EitherT $ execStatement db "SELECT btcAddr, event, eventTime from workEvents"
  rows <- bimapEitherT T.pack id baseResult
  return . intervals . catMaybes $ fmap parseRow (join rows)

parseRow :: Row Value -> Maybe LogEntry
parseRow row = do
  a <- lookup "btcAddr" row >>= valueAddr
  t <- lookup "eventTime" row >>= valueTime
  ev <- lookup "event" row >>= (valueEvent t)
  return $ LogEntry a ev

valueAddr :: Value -> Maybe BtcAddr
valueAddr (Text t) = parseBtcAddr $ T.pack t
valueAddr _ = Nothing

valueTime :: Value -> Maybe UTCTime
valueTime (Text t) = parseTime defaultTimeLocale "%c" t
valueTime _        = Nothing

valueEvent :: UTCTime -> Value -> Maybe WorkEvent
valueEvent t (Text "start") = Just (StartWork t)
valueEvent t (Text "stop")  = Just (StopWork t)
valueEvent _              _ = Nothing

formatSqlTime :: UTCTime -> String
formatSqlTime t = formatTime defaultTimeLocale "%c" t

eventTable :: SQLTable
eventTable = Table "workEvents" [ Column "btcAddr"   (SQLVarChar 256) []
                                , Column "event"     (SQLVarChar 64) []
                                , Column "eventTime" (SQLDateTime DATETIME) [] ] []

