{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.WorkLog where

import Aftok.Database
import Aftok.Http.Json
import Aftok.Json
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Util
import Aftok.TimeLog
  ( AmendmentId,
    EventId (..),
    LogEvent,
    ModTime (..),
    WorkIndex (..),
    workIndex,
  )
import Aftok.Types
  ( ProjectId,
    UserId,
  )
import Control.Lens (view)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import qualified Data.Thyme.Clock as C
import qualified Data.UUID as U
import Snap.Core
import qualified Snap.Snaplet as S

----------------------
-- Handlers
----------------------

logWorkHandler ::
  (C.UTCTime -> LogEvent) ->
  S.Handler App App (ProjectId, UserId, KeyedLogEntry)
logWorkHandler evCtr = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case (eitherDecode requestBody >>= parseEither (parseLogEntry uid evCtr)) of
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
        pure
        ev

amendEventHandler :: S.Handler App App (EventId, AmendmentId)
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
    (parseEither (parseEventAmendment modTime) requestJSON)

projectWorkIndex :: S.Handler App App (WorkIndex KeyedLogEntry)
projectWorkIndex = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid

userEvents :: S.Handler App App [KeyedLogEntry]
userEvents = do
  uid <- requireUserId
  pid <- requireProjectId
  ival <- rangeQueryParam
  limit <- Limit . fromMaybe 1 <$> decimalParam "limit"
  snapEval $ findEvents pid uid ival limit

userWorkIndex :: S.Handler App App (WorkIndex KeyedLogEntry)
userWorkIndex = workIndex (view logEntry) <$> userEvents
