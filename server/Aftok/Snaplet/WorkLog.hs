{-# LANGUAGE TupleSections      #-}
module Aftok.Snaplet.WorkLog where



import           Control.Lens
import           Control.Monad.Trans.Maybe      ( mapMaybeT )

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.Text                     as T
import           Data.Thyme.Clock              as C
import           Data.UUID                     as U
import           Network.Haskoin.Address        ( Address
                                                , stringToAddr
                                                )

import           Aftok.Currency.Bitcoin         ( NetworkId(..)
                                                , toNetwork
                                                )
import           Aftok.Database
import           Aftok.Interval
import           Aftok.Json
import           Aftok.Project
import           Aftok.TimeLog
import           Aftok.Types                    ( CreditTo(..) )
import           Aftok.Util                     ( fromMaybeT )

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth
import           Aftok.Snaplet.Util

import           Snap.Core
import           Snap.Snaplet                  as S

logWorkHandler :: (C.UTCTime -> LogEvent) -> S.Handler App App (EventId, KeyedLogEntry BTCNet)
logWorkHandler evCtr = do
  uid         <- requireUserId
  pid         <- requireProjectId
  nmode       <- getNetworkMode
  requestBody <- readRequestBody 4096
  timestamp   <- liftIO C.getCurrentTime
  case
      A.eitherDecode requestBody >>= A.parseEither (parseLogEntry nmode uid evCtr)
    of
      Left err ->
        snapError 400
          $  "Unable to parse log entry "
          <> (show requestBody)
          <> ": "
          <> show err
      Right entry -> do
        eid <- snapEval $ createEvent pid uid (entry timestamp)
        ev  <- snapEval $ findEvent eid
        maybe
          (snapError 500 $ "An error occured retrieving the newly created event record.")
          (pure . (eid,))
          ev

logWorkBTCHandler :: (C.UTCTime -> LogEvent) -> S.Handler App App EventId
logWorkBTCHandler evCtr = do
  uid   <- requireUserId
  pid   <- requireProjectId
  nmode <- getNetworkMode
  let network = toNetwork nmode BTC
  addrBytes   <- getParam "btcAddr"
  requestBody <- readRequestBody 4096
  timestamp   <- liftIO C.getCurrentTime
  case fmap decodeUtf8 addrBytes >>= stringToAddr network of
    Nothing ->
      snapError 400
        $  "Unable to parse bitcoin address from "
        <> (show addrBytes)
    Just addr -> snapEval . createEvent pid uid $ LogEntry
      (CreditToCurrency (BTC, addr))
      (evCtr timestamp)
      (A.decode requestBody)

loggedIntervalsHandler :: S.Handler App App (WorkIndex (NetworkId, Address))
loggedIntervalsHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid

logEntriesHandler :: S.Handler App App [LogEntry (NetworkId, Address)]
logEntriesHandler = do
  uid       <- requireUserId
  pid       <- requireProjectId
  endpoints <- (,) <$> timeParam "after" <*> timeParam "before"
  ival      <- case endpoints of
    (Just s , Just e ) -> pure $ During s e
    (Nothing, Just e ) -> pure $ Before e
    (Just s , Nothing) -> pure $ After s
    (Nothing, Nothing) -> snapError
      400
      "You must at least one of the \"after\" or \"before\" query parameter"
  snapEval $ findEvents pid uid ival

payoutsHandler :: S.Handler App App (Payouts (NetworkId, Address))
payoutsHandler = do
  uid     <- requireUserId
  pid     <- requireProjectId
  project <- fromMaybeT
    (snapError 400 $ "Project not found for id " <> show pid)
    (mapMaybeT snapEval $ findUserProject uid pid)
  widx  <- snapEval $ readWorkIndex pid uid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts (toDepF $ project ^. depf) ptime widx

amendEventHandler :: S.Handler App App AmendmentId
amendEventHandler = do
  uid          <- requireUserId
  nmode        <- getNetworkMode
  eventIdBytes <- getParam "eventId"
  eventId      <- maybe (snapError 400 "eventId parameter is required")
                        (pure . EventId)
                        (eventIdBytes >>= U.fromASCIIBytes)
  modTime     <- ModTime <$> liftIO C.getCurrentTime
  requestJSON <- readRequestJSON 4096
  either (snapError 400 . T.pack)
         (snapEval . amendEvent uid eventId)
         (A.parseEither (parseEventAmendment nmode modTime) requestJSON)
