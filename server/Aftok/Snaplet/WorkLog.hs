{-# LANGUAGE TupleSections #-}

module Aftok.Snaplet.WorkLog where

import Aftok.Database
import Aftok.Json
import Aftok.Project
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Util
import Aftok.TimeLog
import Aftok.Types
  ( _ProjectId,
    _UserId,
  )
import Aftok.Util (fromMaybeT)
import Control.Lens ((^.))
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.Thyme.Clock as C
import Data.UUID as U
import Snap.Core
import Snap.Snaplet as S

logWorkHandler ::
  (C.UTCTime -> LogEvent) ->
  S.Handler App App (EventId, KeyedLogEntry)
logWorkHandler evCtr = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case (A.eitherDecode requestBody >>= A.parseEither (parseLogEntry uid evCtr)) of
    Left err ->
      snapError 400 $
        "Unable to parse log entry "
          <> (show requestBody)
          <> ": "
          <> show err
    Right entry -> do
      eid <- snapEval $ createEvent pid uid (entry timestamp)
      ev <- snapEval $ findEvent eid
      maybe
        ( snapError 500 $
            "An error occured retrieving the newly created event record."
        )
        (pure . (eid,))
        ev

projectWorkIndex :: S.Handler App App WorkIndex
projectWorkIndex = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid

userEvents :: S.Handler App App [LogEntry]
userEvents = do
  uid <- requireUserId
  pid <- requireProjectId
  ival <- rangeQueryParam
  limit <- Limit . fromMaybe 1 <$> decimalParam "limit"
  snapEval $ findEvents pid uid ival limit

userWorkIndex :: S.Handler App App WorkIndex
userWorkIndex = workIndex <$> userEvents

payoutsHandler :: S.Handler App App FractionalPayouts
payoutsHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  project <-
    fromMaybeT
      (snapError 400 $ "Project not found for id " <> show pid)
      (mapMaybeT snapEval $ findUserProject uid pid)
  widx <- snapEval $ readWorkIndex pid uid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts (toDepF $ project ^. depf) ptime widx

amendEventHandler :: S.Handler App App AmendmentId
amendEventHandler = do
  uid <- requireUserId
  eventIdBytes <- getParam "eventId"
  eventId <-
    maybe
      (snapError 400 "eventId parameter is required")
      (pure . EventId)
      (eventIdBytes >>= U.fromASCIIBytes)
  modTime <- ModTime <$> liftIO C.getCurrentTime
  requestJSON <- readRequestJSON 4096
  either
    (snapError 400 . T.pack)
    (snapEval . amendEvent uid eventId)
    (A.parseEither (parseEventAmendment modTime) requestJSON)

keyedLogEntryJSON :: (EventId, KeyedLogEntry) -> A.Value
keyedLogEntryJSON (eid, (pid, uid, ev)) =
  v2
    . obj
    $ [ "eventId" .= idValue _EventId eid,
        "projectId" .= idValue _ProjectId pid,
        "loggedBy" .= idValue _UserId uid
      ]
      <> logEntryFields ev
