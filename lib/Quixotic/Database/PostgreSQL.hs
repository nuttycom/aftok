{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
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
import Quixotic.Users

eventTypeParser :: FieldParser EventType
eventTypeParser f v = fromField f v >>= nameEvent

uidParser :: FieldParser UserId
uidParser f v = UserId <$> fromField f v

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
qdbUserRowParser = QDBUser <$> fieldWith uidParser <*> userRowParser

-- Local newtypes to permit field serialization 
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

recordEvent' :: UserId -> LogEntry -> ReaderT Connection IO ()
recordEvent' (UserId uid) (LogEntry a e) = do 
  conn <- ask
  void . lift $ execute conn 
    "INSERT INTO work_events (user_id, btc_addr, event_type, event_time) VALUES (?, ?, ?, ?)" 
    ( uid
    , a ^. address
    , e ^. (eventType . to eventName)
    , e ^. eventTime
    )

readWorkIndex' :: ReaderT Connection IO WorkIndex
readWorkIndex' = do
  conn <- ask
  rows <- lift $ query_ conn
    "SELECT btc_addr, event_type, event_time from work_events" 
  pure . workIndex $ fmap pLogEntry rows

newAuction' :: Auction -> ReaderT Connection IO AuctionId
newAuction' auc = do
  conn <- ask
  aucIds <- lift $ query conn
    "INSERT INTO auctions (raise_amount, end_time) VALUES (?, ?) RETURNING id"
    (auc ^. (raiseAmount.to PBTC), auc ^. auctionEnd)
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
    "INSERT INTO bids (auction_id, user_id, bid_seconds, bid_amount, bid_time) values (?, ?, ?, ?, ?)"
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
    "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?) RETURNING id"
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
  }
