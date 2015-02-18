{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database.PostgreSQL (postgresQDB) where

import Blaze.ByteString.Builder (fromByteString)
import ClassyPrelude
import Control.Lens
import Data.Fixed
import Data.Hourglass
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

eventTypeParser :: FieldParser EventType
eventTypeParser f v = fromField f v >>= nameEvent

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


workEventParser :: RowParser WorkEvent
workEventParser = WorkEvent <$> fieldWith eventTypeParser <*> field 

logEntryParser :: RowParser LogEntry
logEntryParser  = LogEntry <$> fieldWith btcAddrParser <*> workEventParser 

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

newtype PQDBProject = PQDBProject { pQDBProject :: QDBProject }
instance FromRow PQDBProject where
  fromRow = PQDBProject <$> qdbProjectRowParser

recordEvent' :: ProjectId -> UserId -> LogEntry -> ReaderT Connection IO ()
recordEvent' (ProjectId pid) (UserId uid) (LogEntry a e) = do 
  conn <- ask
  void . lift $ execute conn 
    "INSERT INTO work_events (project_id, user_id, btc_addr, event_type, event_time) VALUES (?, ?, ?, ?, ?)" 
    ( pid, uid
    , a ^. address
    , e ^. (eventType . to eventName)
    , e ^. eventTime
    )

readWorkIndex' :: ProjectId -> ReaderT Connection IO WorkIndex
readWorkIndex' pid = do
  conn <- ask
  rows <- lift $ query conn
    "SELECT btc_addr, event_type, event_time from work_events WHERE project_id = ?" 
    (Only $ PPid pid)
  pure . workIndex $ fmap pLogEntry rows

newAuction' :: ProjectId -> Auction -> ReaderT Connection IO AuctionId
newAuction' pid auc = do
  conn <- ask
  aucIds <- lift $ query conn
    "INSERT INTO auctions (project_id, raise_amount, end_time) VALUES (?, ?, ?) RETURNING id"
    (pid ^. (_ProjectId), auc ^. (raiseAmount.to PBTC), auc ^. auctionEnd)
  pure . AuctionId . fromOnly $ DL.head aucIds

readAuction' :: AuctionId -> ReaderT Connection IO (Maybe Auction)
readAuction' aucId = do
  conn <- ask
  rows <- lift $ query conn
    "SELECT raise_amount, end_time FROM auctions WHERE ROWID = ?" 
    (Only (aucId ^. _AuctionId))
  pure . fmap pAuction $ headMay rows

recordBid' :: AuctionId -> Bid -> ReaderT Connection IO ()
recordBid' (AuctionId aucId) bid = do
  conn <- ask
  void . lift $ execute conn
    "INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time) values (?, ?, ?, ?, ?)"
    ( aucId 
    , bid ^. (bidUser._UserId)
    , case bid ^. bidSeconds of (Seconds i) -> i
    , bid ^. (bidAmount.to PBTC)
    , bid ^. bidTime
    )

readBids' :: AuctionId -> ReaderT Connection IO [Bid]
readBids' aucId = do
  conn <- ask
  rows <- lift $ query conn
    "SELECT user_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ?"
    (Only $ (aucId ^. _AuctionId))
  pure $ fmap pBid rows

createUser' :: User -> ReaderT Connection IO UserId
createUser' user' = do
  conn <- ask
  uids <- lift $ query conn
    "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?, ?) RETURNING id"
    (user' ^. (username._UserName), user' ^. (userAddress.address), user' ^. userEmail)
  pure . UserId . fromOnly $ DL.head uids

findUser' :: UserId -> ReaderT Connection IO (Maybe User)
findUser' (UserId uid) = do
  conn <- ask
  users <- lift $ query conn
    "SELECT handle, btc_addr, email FROM users WHERE id = ?"
    (Only uid)
  pure . fmap pUser $ headMay users

findUserByUserName' :: UserName -> ReaderT Connection IO (Maybe QDBUser)
findUserByUserName' (UserName h) = do
  conn <- ask
  users <- lift $ query conn
    "SELECT id, handle, btc_addr, email FROM users WHERE handle = ?"
    (Only h)
  pure . fmap pQDBUser $ headMay users

createProject' :: Project -> ReaderT Connection IO ProjectId
createProject' p = do
  let uid = p ^. (initiator._UserId)
  conn <- ask
  pids <- lift $ query conn
    "INSERT INTO projects (project_name, inception_date, initiator_id) VALUES (?, ?, ?) RETURNING id"
    (p ^. projectName, p ^. inceptionDate, uid)
  let pid = fromOnly $ DL.head pids
  void . lift $ execute conn
    "INSERT INTO project_companions (project_id, companion_id) VALUES (?, ?)"
    (pid, uid)
  pure . ProjectId $ pid

findUserProjects' :: UserId -> ReaderT Connection IO [QDBProject]
findUserProjects' (UserId uid) = do
  conn <- ask
  results <- lift $ query conn
    "SELECT p.id, p.project_name, p.inception_date, p.initiator_id \
    \FROM projects p JOIN project_companions pc ON pc.project_id = p.id \
    \WHERE pc.companion_id = ?"
    (Only uid)
  pure $ fmap pQDBProject results


postgresQDB :: QDB (ReaderT Connection IO)
postgresQDB = QDB 
  { recordEvent = recordEvent'
  , readWorkIndex = readWorkIndex' 
  , newAuction = newAuction'
  , readAuction = readAuction'
  , recordBid = recordBid'
  , readBids = readBids'
  , createUser = createUser'
  , findUser = findUser'
  , findUserByUserName = findUserByUserName'
  , createProject = createProject'
  , findUserProjects = findUserProjects'
  }
