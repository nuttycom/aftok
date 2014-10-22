{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Quixotic.Database.SQLite (sqliteADB) where

import Control.Monad
import Control.Monad.Trans.Either
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
  return . intervals $ fmap parseRow (join rows)

parseRow :: Row Value -> LogEntry
parseRow = undefined

formatSqlTime :: UTCTime -> String
formatSqlTime t = formatTime defaultTimeLocale "%c" t

eventTable :: SQLTable
eventTable = Table "workEvents" [ Column "btcAddr"   (SQLVarChar 256) []
                                , Column "event"     (SQLVarChar 64) []
                                , Column "eventTime" (SQLDateTime DATETIME) [] ] []

