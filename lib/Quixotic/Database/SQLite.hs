{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Database.SQLite (sqliteQDB) where

import ClassyPrelude
import Control.Lens
import Control.Monad.Trans.Either
import Data.Text.Lens
import Database.SQLite

import Quixotic
import Quixotic.Auction
import Quixotic.Database
import Quixotic.TimeLog
import Quixotic.Users

sqliteQDB :: SQLiteHandle -> IO (QDB (EitherT Text IO) SQLiteHandle)
sqliteQDB db = do
  _ <- defineTableOpt db True eventTable
  _ <- defineTableOpt db True auctionTable
  return $ QDB 
    { recordEvent = recordEvent'
    , readWorkIndex = readWorkIndex' 
    , newAuction = newAuction'
    , readAuction = readAuction'
    , recordBid = recordBid'
    , readBids = readBids'
    , createUser = createUser'
    }

recordEvent' :: LogEntry -> ReaderT SQLiteHandle (EitherT Text IO) ()
recordEvent' (LogEntry ba ev) = do 
  db <- ask
  lift . lift . void $ insertRow db "workEvents" 
    [ ("btcAddr", ba ^. address ^. from packed)
    , ("event", unpack (eventName ev))
    , ("eventTime", formatSqlTime (logTime ev)) 
    ]

readWorkIndex' :: ReaderT SQLiteHandle (EitherT Text IO) WorkIndex
readWorkIndex' = do
  db <- ask
  let selection = execStatement db "SELECT btcAddr, event, eventTime from workEvents" 
  rows <- lift . EitherT $ fmap (over _Left pack) selection
  return . intervals . catMaybes $ fmap parseLogEntry (join rows)

newAuction' :: Auction -> ReaderT SQLiteHandle (EitherT Text IO) AuctionId
newAuction' a = do
  db <- ask
  _  <-  lift . lift $ insertRow db "auctions"
    [ ("raiseAmount", show $ a ^. (raiseAmount . btc))
    , ("eventTime", formatSqlTime $ a ^. endsAt) 
    ]
  lift . lift . fmap (AuctionId . fromInteger) $ getLastRowID db

readAuction' :: AuctionId -> ReaderT a (EitherT Text IO) Auction
readAuction' (AuctionId aid) = do
  db <- ask
  let selection = execParamStatement db 
                  "SELECT raiseAmount, endsAt FROM auctions WHERE ROWID = :aid" 
                  [("aid", Int aid)]
  rows <- lift . EitherT $ fmap (over _Left pack) selection


recordBid' :: UTCTime -> Bid -> ReaderT a (EitherT Text IO) ()
recordBid' = undefined

readBids' :: AuctionId -> ReaderT a (EitherT Text IO) [(UTCTime, Bid)]
readBids' = undefined

createUser' :: User -> ReaderT a (EitherT Text IO) UserId
createUser' = undefined

parseLogEntry :: Row Value -> Maybe LogEntry
parseLogEntry row = do
  a <- lookup "btcAddr" row >>= valueAddr
  t <- lookup "eventTime" row >>= valueTime
  ev <- lookup "event" row >>= (valueEvent t)
  return $ LogEntry a ev

parseAuction :: Row Value -> Maybe Auction
parseAuction row = 
  Auction <$> (lookup "raiseAmount" row >>= valueBTC)
          <*> (lookup "endsAt" row >>= valueTime)

valueBTC :: Value -> Maybe BTC
valueBTC (Int i) _ = Just $ BTC i
valueBTC _ = Nothing

valueAddr :: Value -> Maybe BtcAddr
valueAddr (Text t) = parseBtcAddr $ pack t
valueAddr _ = Nothing

valueTime :: Value -> Maybe UTCTime
valueTime (Text t) = parseTime defaultTimeLocale "%c" t
valueTime _        = Nothing

valueEvent :: UTCTime -> Value -> Maybe WorkEvent
valueEvent t (Text "start") = Just (StartWork t)
valueEvent t (Text "stop")  = Just (StopWork t)
valueEvent _              _ = Nothing

formatSqlTime :: UTCTime -> String
formatSqlTime t = formatTime defaultTimeLocale "%c" t

eventTable :: SQLTable
eventTable = Table "workEvents" 
  [ Column "btcAddr"   (SQLVarChar 256) []
  , Column "event"     (SQLVarChar 64) []
  , Column "eventTime" (SQLDateTime DATETIME) [] 
  ] []

auctionTable :: SQLTable
auctionTable = Table "auctions" 
  [ Column "raiseAmouont" (SQLInt BIG False False) []
  , Column "endsAt"       (SQLDateTime DATETIME) [] 
  ] []

