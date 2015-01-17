{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Trans.Reader
import qualified Data.Aeson as A
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.Map
import Database.SQLite.Simple

import Quixotic
import Quixotic.Database
import Quixotic.Database.SQLite
import Quixotic.Json
import Quixotic.TimeLog

import Snap.Core
import Snap.Http.Server
import qualified Snap.Http.Server.Config as SC

main :: IO ()
main = do
  cfg <- loadQConfig "quixotic.cfg"
  db  <- open $ dbName cfg
  sconf <- snapConfig cfg
  simpleHttpServe sconf $ runReaderT (site sqliteQDB) db

site :: QDB IO a -> ReaderT a Snap ()
site qdb = route 
  [ ("logStart/:btcAddr", logWorkHandler qdb StartWork)
  , ("logEnd/:btcAddr",   logWorkHandler qdb StopWork)
  , ("loggedIntervals/:btcAddr", loggedIntervalsHandler qdb)
  , ("payouts", payoutsHandler qdb)
  ] 

data QConfig = QConfig
  { hostname :: ByteString
  , port :: Int
  -- , sslCert :: FilePath
  -- , sslKey :: FilePath
  , dbName :: String
  } 

loadQConfig :: FilePath -> IO QConfig
loadQConfig cfgFile = do 
  cfg <- C.load [C.Required (fpToString cfgFile)]
  parseQConfig cfg

parseQConfig :: CT.Config -> IO QConfig
parseQConfig cfg = 
  QConfig <$> C.lookupDefault "localhost" cfg "hostname"
          <*> C.lookupDefault 8000 cfg "port" 
          -- <*> (fmap fpFromText $ C.require cfg "sslCert")
          -- <*> (fmap fpFromText $ C.require cfg "sslKey")
          <*> C.require cfg "db" 

baseSnapConfig :: MonadSnap m => QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig cfg = 
  SC.setHostname (hostname cfg) . 
  SC.setPort (port cfg) 
  --SC.setSSLPort (port cfg) .
  --SC.setSSLCert (fpToString $ sslCert cfg) .
  --SC.setSSLKey (fpToString $ sslKey cfg)

snapConfig :: QConfig -> IO (SC.Config Snap ())
snapConfig cfg = SC.commandLineConfig $ baseSnapConfig cfg emptyConfig

logWorkHandler :: QDB IO a -> EventType -> ReaderT a Snap ()
logWorkHandler qdb ev = do 
  let QDB{..} = qdb
  addrBytes <- lift $ getParam "btcAddr"
  timestamp <- lift $ liftIO getCurrentTime
  maybe 
    (lift $ snapError 400 "")
    (\a -> mapReaderT liftIO $ recordEvent (LogEntry a (WorkEvent ev timestamp)))
    (fmap decodeUtf8 addrBytes >>= parseBtcAddr)

loggedIntervalsHandler :: QDB IO a -> ReaderT a Snap ()
loggedIntervalsHandler qdb = do
  let QDB{..} = qdb
  widx <- mapReaderT liftIO $ readWorkIndex
  lift . modifyResponse $ addHeader "content-type" "application/json"
  lift . writeLBS . A.encode $ mapKeys (^. address) widx

payoutsHandler :: QDB IO a -> ReaderT a Snap ()
payoutsHandler qdb = do 
  let QDB{..} = qdb
      dep = linearDepreciation (Months 6) (Months 60) 
  ptime <- lift . liftIO $ getCurrentTime
  widx <- mapReaderT liftIO $ readWorkIndex
  lift . modifyResponse $ addHeader "content-type" "application/json"
  lift . writeLBS . A.encode . PayoutsResponse $ payouts dep ptime widx

snapError :: Int -> Text -> Snap ()
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  r <- getResponse
  finishWith r
