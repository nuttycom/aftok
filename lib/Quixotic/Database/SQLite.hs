{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Database.SQLite (sqliteADB) where

import ClassyPrelude
import Control.Monad.Trans.Either
import Database.SQLite

import Quixotic
import qualified Quixotic.Database as D
import Quixotic.TimeLog

sqliteADB :: SQLiteHandle -> IO (D.ADB IO SQLiteHandle)
sqliteADB db = do
  _ <- defineTableOpt db True eventTable
  return $ D.ADB recordEvent readWorkIndex 

recordEvent :: SQLiteHandle -> LogEntry -> IO ()
recordEvent h (LogEntry ba ev) = 
  void $ insertRow h "workEvents" [ ("btcAddr", unpack (address ba))
                                  , ("event", unpack (eventName ev))
                                  , ("eventTime", formatSqlTime (logTime ev)) ]

readWorkIndex :: SQLiteHandle -> EitherT Text IO WorkIndex
readWorkIndex db = do
  let baseResult = EitherT $ execStatement db "SELECT btcAddr, event, eventTime from workEvents"
  rows <- bimapEitherT pack id baseResult
  return . intervals . catMaybes $ fmap parseRow (join rows)

parseRow :: Row Value -> Maybe LogEntry
parseRow row = do
  a <- lookup "btcAddr" row >>= valueAddr
  t <- lookup "eventTime" row >>= valueTime
  ev <- lookup "event" row >>= (valueEvent t)
  return $ LogEntry a ev

valueAddr :: Value -> Maybe BtcAddr
valueAddr (Text t) = parseBtcAddr $ pack t
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
