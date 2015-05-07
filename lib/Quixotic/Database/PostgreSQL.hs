{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database.PostgreSQL (postgresQDB) where

import Blaze.ByteString.Builder (fromByteString)
import ClassyPrelude
import Control.Lens
import Data.ByteString.Char8 as B
import Data.Fixed
import Data.Hourglass
import Data.Thyme.Clock as C
import Data.Thyme.Time
import qualified Data.List as DL
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Network.Bitcoin

import Quixotic
import Quixotic.Auction
import Quixotic.Database
import Quixotic.TimeLog

type QDBM = ReaderT Connection IO

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


workEventParser :: RowParser LogEvent
workEventParser = fieldWith eventTypeParser <*> fieldWith utcParser

logEntryParser :: RowParser LogEntry
logEntryParser  = LogEntry <$> fieldWith btcAddrParser <*> workEventParser <*> field

auctionRowParser :: RowParser Auction
auctionRowParser = Auction <$> fieldWith btcParser <*> field

bidRowParser :: RowParser Bid
bidRowParser = Bid <$> fieldWith uidParser 
                   <*> fieldWith secondsParser 
                   <*> fieldWith btcParser 
                   <*> field

userRowParser :: RowParser User
userRowParser = User <$> fieldWith usernameParser
                     <*> fieldWith btcAddrParser
                     <*> field

qdbUserRowParser :: RowParser QDBUser
qdbUserRowParser = QDBUser <$> fieldWith uidParser 
                           <*> userRowParser

projectRowParser :: RowParser Project
projectRowParser = Project <$> field 
                           <*> field 
                           <*> fieldWith uidParser

qdbProjectRowParser :: RowParser QDBProject
qdbProjectRowParser = QDBProject <$> fieldWith pidParser <*> projectRowParser


-- Local newtypes to permit field serialization 
newtype PPid = PPid ProjectId
instance ToField PPid where
  toField (PPid (ProjectId i)) = toField i

newtype PBTC = PBTC BTC 
instance ToField PBTC where
  toField (PBTC btc) = Plain . fromByteString . fromString $ showFixed False btc

-- Local newtypes to permit row deserialization via
-- typeclass. Wish I could just pass the RowParser instances

newtype PLogEntry = PLogEntry { pLogEntry :: LogEntry } 
instance FromRow PLogEntry where
  fromRow = PLogEntry <$> logEntryParser

newtype PBid = PBid { pBid :: Bid }
instance FromRow PBid where
  fromRow = PBid <$> bidRowParser

newtype PUser = PUser { pUser :: User }
instance FromRow PUser where
  fromRow = PUser <$> userRowParser

newtype PAuction = PAuction { pAuction :: Auction }
instance FromRow PAuction where
  fromRow = PAuction <$> auctionRowParser

newtype PQDBUser = PQDBUser { pQDBUser :: QDBUser }
instance FromRow PQDBUser where
  fromRow = PQDBUser <$> qdbUserRowParser

newtype PProject = PProject { pProject :: Project }
instance FromRow PProject where
  fromRow = PProject <$> projectRowParser

newtype PQDBProject = PQDBProject { pQDBProject :: QDBProject }
instance FromRow PQDBProject where
  fromRow = PQDBProject <$> qdbProjectRowParser

pquery :: (ToRow d, FromRow r) => Query -> d -> QDBM [r]
pquery q d = do
  conn <- ask
  lift $ query conn q d

pexec :: (ToRow d) => Query -> d -> QDBM Int64
pexec q d = do
  conn <- ask
  lift $ execute conn q d 

createEvent' :: ProjectId -> UserId -> LogEntry -> QDBM EventId
createEvent' (ProjectId pid) (UserId uid) (LogEntry a e m) = do 
  eventIds <- pquery
    "INSERT INTO work_events (project_id, user_id, btc_addr, event_type, event_time, event_metadata) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \RETURNING id" 
    ( pid, uid
    , a ^. _BtcAddr
    , eventName e
    , fromThyme $ eventTime e
    , m
    )
  pure . EventId . fromOnly $ DL.head eventIds

amendEvent' :: EventId -> LogModification -> QDBM ()
amendEvent' (EventId eid) (TimeChange mt t) = 
  void $ pexec
    "INSERT INTO event_time_amendments (event_id, mod_time, event_time) VALUES (?, ?, ?)"
    ( eid, fromThyme $ mt ^. _ModTime, fromThyme t )

amendEvent' (EventId eid) (AddressChange mt addr) = 
  void $ pexec
    "INSERT INTO event_addr_amendments (event_id, mod_time, btc_addr) VALUES (?, ?, ?)"
    ( eid, fromThyme $ mt ^. _ModTime, addr ^. _BtcAddr )
  
amendEvent' (EventId eid) (MetadataChange mt v) = 
  void $ pexec
    "INSERT INTO event_metadata_amendments (event_id, mod_time, btc_addr) VALUES (?, ?, ?)"
    ( eid, fromThyme $ mt ^. _ModTime, v )

readWorkIndex' :: ProjectId -> QDBM WorkIndex
readWorkIndex' pid = do
  rows <- pquery
    "SELECT btc_addr, event_type, event_time, event_metadata from work_events WHERE project_id = ?" 
    (Only $ PPid pid)
  pure . workIndex $ fmap pLogEntry rows

createAuction' :: ProjectId -> Auction -> QDBM AuctionId
createAuction' pid auc = do
  aucIds <- pquery
    "INSERT INTO auctions (project_id, raise_amount, end_time) VALUES (?, ?, ?) RETURNING id"
    (pid ^. (_ProjectId), auc ^. (raiseAmount.to PBTC), auc ^. auctionEnd)
  pure . AuctionId . fromOnly $ DL.head aucIds

findAuction' :: AuctionId -> QDBM (Maybe Auction)
findAuction' aucId = do
  rows <- pquery
    "SELECT raise_amount, end_time FROM auctions WHERE ROWID = ?" 
    (Only (aucId ^. _AuctionId))
  pure . fmap pAuction $ headMay rows

createBid' :: AuctionId -> Bid -> QDBM ()
createBid' (AuctionId aucId) bid = do
  void $ pexec
    "INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time) values (?, ?, ?, ?, ?)"
    ( aucId 
    , bid ^. (bidUser._UserId)
    , case bid ^. bidSeconds of (Seconds i) -> i
    , bid ^. (bidAmount.to PBTC)
    , bid ^. bidTime
    )

readBids' :: AuctionId -> QDBM [Bid]
readBids' aucId = do
  rows <- pquery
    "SELECT user_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ?"
    (Only $ (aucId ^. _AuctionId))
  pure $ fmap pBid rows

createUser' :: User -> QDBM UserId
createUser' user' = do
  uids <- pquery
    "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?, ?) RETURNING id"
    (user' ^. (username._UserName), user' ^. (userAddress._BtcAddr), user' ^. userEmail)
  pure . UserId . fromOnly $ DL.head uids

findUser' :: UserId -> QDBM (Maybe User)
findUser' (UserId uid) = do
  users <- pquery
    "SELECT handle, btc_addr, email FROM users WHERE id = ?"
    (Only uid)
  pure . fmap pUser $ headMay users

findUserByUserName' :: UserName -> QDBM (Maybe QDBUser)
findUserByUserName' (UserName h) = do
  users <- pquery
    "SELECT id, handle, btc_addr, email FROM users WHERE handle = ?"
    (Only h)
  pure . fmap pQDBUser $ headMay users

createProject' :: Project -> QDBM ProjectId
createProject' p = do
  let uid = p ^. (initiator._UserId)
  pids <- pquery
    "INSERT INTO projects (project_name, inception_date, initiator_id) VALUES (?, ?, ?) RETURNING id"
    (p ^. projectName, p ^. inceptionDate, uid)
  let pid = fromOnly $ DL.head pids
  void $ pexec
    "INSERT INTO project_companions (project_id, companion_id) VALUES (?, ?)"
    (pid, uid)
  pure . ProjectId $ pid

findProject' :: ProjectId -> QDBM (Maybe Project)
findProject' (ProjectId pid) = do
  projects <- pquery
    "SELECT project_name, inception_date, initiator_id FROM projects WHERE id = ?"
    (Only pid)
  pure . fmap pProject $ headMay projects

findUserProjects' :: UserId -> QDBM [QDBProject]
findUserProjects' (UserId uid) = do
  results <- pquery
    "SELECT p.id, p.project_name, p.inception_date, p.initiator_id \
    \FROM projects p JOIN project_companions pc ON pc.project_id = p.id \
    \WHERE pc.companion_id = ?"
    (Only uid)
  pure $ fmap pQDBProject results

postgresQDB :: QDB QDBM
postgresQDB = QDB 
  { createEvent = createEvent'
  , amendEvent = amendEvent'
  , readWorkIndex = readWorkIndex' 

  , createAuction = createAuction'
  , findAuction = findAuction'

  , createBid = createBid'
  , readBids = readBids'

  , createUser = createUser'
  , findUser = findUser'
  , findUserByUserName = findUserByUserName'

  , createProject = createProject'
  , findProject = findProject'
  , findUserProjects = findUserProjects'
  }
