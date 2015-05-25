module Aftok.Snaplet.WorkLog where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.UUID as U
import Data.Thyme.Clock as C

import Aftok
import Aftok.Database
import Aftok.Interval
import Aftok.Json
import Aftok.TimeLog

import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Util

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

-- TODO: ignore "duplicate" events within some small time limit?
logWorkHandler :: (C.UTCTime -> LogEvent) -> Handler App App EventId
logWorkHandler evCtr = do 
  QDB{..} <- view qdb <$> with qm get
  (uid, pid) <- requireProjectAccess
  addrBytes <- getParam "btcAddr"
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case fmap decodeUtf8 addrBytes >>= parseBtcAddr of
    Nothing -> snapError 400 $ "Unable to parse bitcoin address from " <> (tshow addrBytes)
    Just addr -> 
      let logEntry a = LogEntry a (evCtr timestamp) (A.decode requestBody)
          storeEv  a = runReaderT . createEvent pid uid $ logEntry a
      in  liftPG $ storeEv addr

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
    (Nothing, Nothing) -> snapError 400 "You must at least one of the \"after\" or \"before\" query parameter"
  liftPG . runReaderT $ findEvents pid uid ival

payoutsHandler :: Handler App App Payouts
payoutsHandler = do 
  (QModules QDB{..} df) <- with qm get
  pid   <- fmap snd requireProjectAccess
  widx  <- liftPG . runReaderT $ readWorkIndex pid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts df ptime widx

amendEventHandler :: Handler App App AmendmentId
amendEventHandler = do
  QDB{..} <- view qdb <$> with qm get
  (uid, _) <- requireProjectAccess
  eventIdBytes <- getParam "eventId"
  eventId <- maybe 
    (snapError 400 "eventId parameter is required") 
    (pure . EventId) 
    (eventIdBytes >>= U.fromASCIIBytes)

  ev <- liftPG . runReaderT $ findEvent eventId
  (_, _, uid', _) <- maybe (snapError 404 ("Event not found for id " <> tshow eventId)) pure ev

  modTime <- ModTime <$> liftIO C.getCurrentTime
  requestJSON <- readRequestJSON 4096
  if uid' == uid
    then either 
      (snapError 400 . pack)
      (liftPG . runReaderT . amendEvent eventId)
      (parseEither (parseEventAmendment modTime) requestJSON)
    else
      (snapError 403 "You do not have permission to view this event.")
