module Quixotic.Snaplet.WorkLog where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import qualified Data.Aeson as A
import Data.Thyme.Clock as C

import Quixotic
import Quixotic.Database
import Quixotic.Interval
import Quixotic.TimeLog

import Quixotic.Snaplet
import Quixotic.Snaplet.Auth
import Quixotic.Snaplet.Util

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

logWorkHandler :: (C.UTCTime -> LogEvent) -> Handler App App EventId
logWorkHandler evCtr = do 
  QDB{..} <- view qdb <$> with qm get
  (uid, pid) <- requireProjectAccess
  addrBytes <- getParam "btcAddr"
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  let logEntry addr = LogEntry addr (evCtr timestamp) (A.decode requestBody)
      storeEv addr  = runReaderT . createEvent pid uid $ logEntry addr
  case fmap decodeUtf8 addrBytes >>= parseBtcAddr of
    Nothing -> snapError 400 $ "Unable to parse bitcoin address from " <> (tshow addrBytes)
    Just addr -> liftPG $ storeEv addr

loggedIntervalsHandler :: Handler App App WorkIndex
loggedIntervalsHandler = do
  QDB{..} <- view qdb <$> with qm get
  pid <- fmap snd requireProjectAccess
  liftPG . runReaderT $ readWorkIndex pid

logEntriesHandler :: Handler App App [LogEntry]
logEntriesHandler = do
  QDB{..} <- view qdb <$> with qm get
  (uid, pid) <- requireProjectAccess
  endpoints <- (,) <$> timeParam "after" <*> timeParam "before"
  ival <- case endpoints of
    (Just s,  Just e)  -> pure $ During s e
    (Nothing, Just e)  -> pure $ Before e
    (Just s,  Nothing) -> pure $ After s
    (Nothing, Nothing) -> snapError 400 $ "You must at least one of the \"after\" or \"before\" query parameter"
  liftPG . runReaderT $ findEvents pid uid ival

payoutsHandler :: Handler App App Payouts
payoutsHandler = do 
  (QModules QDB{..} df) <- with qm get
  pid <- fmap snd requireProjectAccess
  widx <- liftPG . runReaderT $ readWorkIndex pid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts df ptime widx

-- amendEventHandler :: Handler App App AmendmentId

