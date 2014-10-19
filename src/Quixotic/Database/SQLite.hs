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
import System.Locale

sqliteADB :: String -> IO (D.ADB IO SQLiteHandle)
sqliteADB dbName = do
  db <- openConnection "quixotic.db"
  return $ D.ADB recordEvent readWorkIndex 

recordEvent :: SQLiteHandle -> LogEntry -> IO ()
recordEvent h (LogEntry ba ev) = 
  void $ insertRow h "workEvents" [ ("btcAddr", T.unpack (address ba))
                                  , ("event", eventName ev)
                                  , ("eventTime", formatSqlTime (logTime ev)) ]

readWorkIndex :: SQLiteHandle -> EitherT String IO WorkIndex
readWorkIndex db = do
  rows <- EitherT $ execStatement db "SELECT btcAddr, event, eventTime from workEvents"
  undefined

formatSqlTime :: UTCTime -> String
formatSqlTime t = formatTime defaultTimeLocale "%c" t

eventTable :: SQLTable
eventTable = Table "workEvents" [ Column "btcAddr"   (SQLVarChar 256) []
                                , Column "event"     (SQLVarChar 64) []
                                , Column "eventTime" (SQLDateTime DATETIME) [] ] []

