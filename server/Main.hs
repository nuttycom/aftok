{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Trans.Reader
import qualified Data.Aeson as A
import qualified Data.Configurator as C
import Data.Map
import qualified Data.Text as T
import Database.SQLite.Simple
import Quixotic
import Quixotic.Database
import Quixotic.Database.SQLite
import Quixotic.TimeLog
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = do
  cfg <- parseConfig "quixotic.cfg"
  db  <- open $ dbName cfg
  quickHttpServe $ runReaderT (site sqliteQDB) db

site :: QDB IO a -> ReaderT a Snap ()
site qdb = route 
  [ ("logStart/:btcAddr", handleLogRequest qdb StartWork)
  , ("logEnd/:btcAddr",   handleLogRequest qdb StopWork)
  , ("payouts", currentPayouts qdb)
  ] 

data QConfig = QConfig
  { port :: Int
  , dbName :: String
  } 

parseConfig :: FilePath -> IO QConfig
parseConfig cfgFile = do 
  cfg <- C.load [C.Required (fpToString cfgFile)]
  QConfig <$> C.require cfg "port" <*> C.require cfg "db" 

handleLogRequest :: QDB IO a -> EventType -> ReaderT a Snap ()
handleLogRequest qdb ev = do 
  let QDB{..} = qdb
  addrBytes <- lift $ getParam "btcAddr"
  timestamp <- lift $ liftIO getCurrentTime
  maybe 
    (lift $ snapError 400 "")
    (\a -> mapReaderT liftIO $ recordEvent (LogEntry a (WorkEvent ev timestamp)))
    (fmap decodeUtf8 addrBytes >>= parseBtcAddr)

currentPayouts :: QDB IO a -> ReaderT a Snap ()
currentPayouts qdb = do 
  let QDB{..} = qdb
      dep = linearDepreciation (Months 6) (Months 60) 

  ptime <- lift . liftIO $ getCurrentTime
  widx <- mapReaderT liftIO $ readWorkIndex
  lift . writeLBS . A.encode . PayoutsResponse $ payouts dep ptime widx

snapError :: Int -> Text -> Snap ()
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  r <- getResponse
  finishWith r

newtype PayoutsResponse = PayoutsResponse Payouts

instance A.ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = A.toJSON m where
    m :: Map T.Text Double
    m = fmap fromRational $ mapKeys (^. address) p
