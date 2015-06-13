{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Aftok.Database.PostgreSQL (QDBM(..)) where

import Blaze.ByteString.Builder (fromByteString)
import ClassyPrelude
import Control.Lens
import Data.Aeson(toJSON)
import qualified Data.ByteString.Char8 as B
import Data.Fixed
import Data.List as L
import Data.Hourglass
import Data.Thyme.Clock as C
import Data.Thyme.Time
import Data.UUID(UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Network.Bitcoin

import Aftok
import Aftok.Auction
import Aftok.Database
import Aftok.Interval
import Aftok.TimeLog

newtype QDBM a = QDBM { runQDBM :: ReaderT Connection IO a }
  deriving (Functor, Applicative, Monad)

eventTypeParser :: FieldParser (C.UTCTime -> LogEvent)
eventTypeParser f v = do
  tn <- typename f
  case tn of 
    "event_t" -> 
      let err = UnexpectedNull (B.unpack tn)
                               (tableOid f) 
                               (maybe "" B.unpack (name f))
                               "UTCTime -> LogEvent"
                               "columns of type event_t should not contain null values"
      in  maybe (conversionError err) (nameEvent . decodeUtf8) v
    _ -> 
      let err = Incompatible (B.unpack tn)
                             (tableOid f) 
                             (maybe "" B.unpack (name f))
                             "UTCTime -> LogEvent"
                             "column was not of type event_t"
      in conversionError err

uidParser :: FieldParser UserId
uidParser f v = UserId <$> fromField f v

pidParser :: FieldParser ProjectId
pidParser f v = ProjectId <$> fromField f v

secondsParser :: FieldParser Seconds
secondsParser f v = Seconds <$> fromField f v

usernameParser :: FieldParser UserName
usernameParser f v = UserName <$> fromField f v

btcAddrParser :: FieldParser BtcAddr
btcAddrParser f v = BtcAddr <$> fromField f v

btcParser :: FieldParser BTC
btcParser f v = fromRational <$> fromField f v

utcParser :: FieldParser C.UTCTime
utcParser f v = toThyme <$> fromField f v 

newtype PPid = PPid ProjectId
instance ToField PPid where
  toField (PPid (ProjectId i)) = toField i

newtype PBTC = PBTC BTC 
instance ToField PBTC where
  toField (PBTC btc) = Plain . fromByteString . fromString $ showFixed False btc

newtype PUTCTime = PUTCTime C.UTCTime 
instance ToField PUTCTime where
  toField (PUTCTime t) = toField $ fromThyme t

logEntryParser :: RowParser LogEntry
logEntryParser = 
  LogEntry <$> fieldWith btcAddrParser 
           <*> (fieldWith eventTypeParser <*> fieldWith utcParser)
           <*> field

qdbLogEntryParser :: RowParser KeyedLogEntry
qdbLogEntryParser = 
  (,,) <$> fieldWith pidParser
        <*> fieldWith uidParser 
        <*> logEntryParser
  
auctionParser :: RowParser Auction
auctionParser = 
  Auction <$> fieldWith btcParser 
          <*> field

bidParser :: RowParser Bid
bidParser = 
  Bid <$> fieldWith uidParser 
      <*> fieldWith secondsParser 
      <*> fieldWith btcParser 
      <*> field

userParser :: RowParser User
userParser = 
  User <$> fieldWith usernameParser
       <*> fieldWith btcAddrParser
       <*> field

qdbUserParser :: RowParser KeyedUser
qdbUserParser = 
  (,) <$> fieldWith uidParser 
      <*> userParser

projectParser :: RowParser Project
projectParser = 
  Project <$> field 
          <*> field 
          <*> fieldWith uidParser
          <*> fieldWith fromJSONField

qdbProjectParser :: RowParser KeyedProject
qdbProjectParser = 
  (,) <$> fieldWith pidParser 
      <*> projectParser

pexec :: (ToRow d) => Query -> d -> QDBM Int64 
pexec q d = QDBM $ do
  conn <- ask
  lift $ execute conn q d

pinsert :: (ToRow d) => (UUID -> r) -> Query -> d -> QDBM r 
pinsert f q d = QDBM $ do
  conn <- ask
  ids  <- lift $ query conn q d
  pure . f . fromOnly $ L.head ids

pquery :: (ToRow d) => RowParser r -> Query -> d -> QDBM [r]
pquery p q d = QDBM $ do
  conn <- ask
  lift $ queryWith p conn q d

instance DBEval QDBM where
  dbEval (CreateEvent (ProjectId pid) (UserId uid) (LogEntry a e m)) = 
    pinsert EventId
      "INSERT INTO work_events (project_id, user_id, btc_addr, event_type, event_time, event_metadata) \
      \VALUES (?, ?, ?, ?, ?, ?) \
      \RETURNING id" 
      ( pid, uid
      , a ^. _BtcAddr
      , eventName e
      , fromThyme $ e ^. eventTime
      , m
      )

  dbEval (FindEvent (EventId eid)) = do 
    logEntries <- pquery qdbLogEntryParser
      "SELECT project_id, user_id, btc_addr, event_type, event_time, event_metadata FROM work_events \
      \WHERE id = ?"
      (Only eid)
    pure $ headMay logEntries

  dbEval (FindEvents (ProjectId pid) (UserId uid) ival) =
    let q p (Before e) = pquery p
          "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
          \WHERE project_id = ? AND user_id = ? AND event_time <= ?" 
          (pid, uid, PUTCTime e)
        q p (During s e) = pquery p
          "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
          \WHERE project_id = ? AND user_id = ? \
          \AND event_time >= ? AND event_time <= ?" 
          (pid, uid, PUTCTime s, PUTCTime e)
        q p (After s) = pquery p
          "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
          \WHERE project_id = ? AND user_id = ? AND event_time >= ?" 
          (pid, uid, PUTCTime s)
    in  q logEntryParser ival

  dbEval (AmendEvent (EventId eid) (TimeChange mt t)) = 
    pinsert AmendmentId
      "INSERT INTO event_time_amendments (event_id, mod_time, event_time) VALUES (?, ?, ?) RETURNING id"
      ( eid, fromThyme $ mt ^. _ModTime, fromThyme t )

  dbEval (AmendEvent (EventId eid) (AddressChange mt addr)) = 
    pinsert AmendmentId
      "INSERT INTO event_addr_amendments (event_id, mod_time, btc_addr) VALUES (?, ?, ?) RETURNING id"
      ( eid, fromThyme $ mt ^. _ModTime, addr ^. _BtcAddr )
  
  dbEval (AmendEvent (EventId eid) (MetadataChange mt v)) = 
    pinsert AmendmentId
      "INSERT INTO event_metadata_amendments (event_id, mod_time, btc_addr) VALUES (?, ?, ?) RETURNING id"
      ( eid, fromThyme $ mt ^. _ModTime, v)

  dbEval (ReadWorkIndex pid) = do
    logEntries <- pquery logEntryParser
      "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events WHERE project_id = ?" 
      (Only $ PPid pid)
    pure $ workIndex logEntries

  dbEval (CreateAuction pid auc) = 
    pinsert AuctionId
      "INSERT INTO auctions (project_id, raise_amount, end_time) \
      \VALUES (?, ?, ?) RETURNING id"
      (pid ^. (_ProjectId), auc ^. (raiseAmount.to PBTC), auc ^. auctionEnd)

  dbEval (FindAuction aucId) = do
    auctions <- pquery auctionParser
      "SELECT raise_amount, end_time FROM auctions WHERE id = ?" 
      (Only (aucId ^. _AuctionId))
    pure $ headMay auctions

  dbEval (CreateBid (AuctionId aucId) bid) = 
    pinsert BidId
      "INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time) \
      \VALUES (?, ?, ?, ?, ?) RETURNING id"
      ( aucId 
      , bid ^. (bidUser._UserId)
      , case bid ^. bidSeconds of (Seconds i) -> i
      , bid ^. (bidAmount.to PBTC)
      , bid ^. bidTime
      )

  dbEval (ReadBids aucId) = 
    pquery bidParser
      "SELECT user_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ?"
      (Only $ (aucId ^. _AuctionId))

  dbEval (CreateUser user') = 
    pinsert UserId
      "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?, ?) RETURNING id"
      (user' ^. (username._UserName), user' ^. (userAddress._BtcAddr), user' ^. userEmail)

  dbEval (FindUser (UserId uid)) = do
    users <- pquery userParser
      "SELECT handle, btc_addr, email FROM users WHERE id = ?"
      (Only uid)
    pure $ headMay users

  dbEval (FindUserByName (UserName h)) = do
    users <- pquery qdbUserParser
      "SELECT id, handle, btc_addr, email FROM users WHERE handle = ?"
      (Only h)
    pure $ headMay users

  dbEval (CreateProject p) = 
    pinsert ProjectId
      "INSERT INTO projects (project_name, inception_date, initiator_id, depreciation_fn) \
      \VALUES (?, ?, ?, ?) RETURNING id"
      (p ^. projectName, p ^. inceptionDate, p ^. (initiator._UserId), toJSON $ p ^. depf)

  dbEval (FindProject (ProjectId pid)) = do
    projects <- pquery projectParser
      "SELECT project_name, inception_date, initiator_id FROM projects WHERE id = ?"
      (Only pid)
    pure $ headMay projects

  dbEval (FindUserProjects (UserId uid)) = 
    pquery qdbProjectParser
      "SELECT p.id, p.project_name, p.inception_date, p.initiator_id \
      \FROM projects p JOIN project_companions pc ON pc.project_id = p.id \
      \WHERE pc.user_id = ? \
      \UNION \
      \SELECT p.id, p.project_name, p.inception_date, p.initiator_id \
      \FROM projects p \
      \WHERE p.initiator_id = ?"
      (uid, uid)

  dbEval (AddUserToProject pid current new) = do
    void $ pexec
      "INSERT INTO project_companions (project_id, user_id, invited_by) VALUES (?, ?, ?)"
      (pid ^. _ProjectId, new ^. _UserId, current ^. _UserId)

  -- FIXME, these are just placeholders
  dbEval (OpForbidden _ reason _) = fail $ show reason
  dbEval (SubjectNotFound _) = fail "Subject of operation was not found."
