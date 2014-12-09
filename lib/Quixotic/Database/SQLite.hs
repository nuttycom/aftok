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
  let baseResult = EitherT $ execStatement db "SELECT btcAddr, event, eventTime from workEvents"
  rows <- lift $ bimapEitherT pack id baseResult
  return . intervals . catMaybes $ fmap parseRow (join rows)

newAuction' :: Auction -> ReaderT a (EitherT Text IO) AuctionId
newAuction' a = do
  db <- ask
  _  <- lift . lift $ insertRow db "auctions"
    [ ("raiseAmount", a ^. (raiseAmount . btc))
    , ("eventTime", formatSqlTime $ a ^. endsAt) 
    ]
  lift . lift . (fmap AuctionId) getLastRowID

readAuction' :: AuctionId -> ReaderT a (EitherT Text IO) Auction
readAuction' = undefined

recordBid' :: UTCTime -> Bid -> ReaderT a (EitherT Text IO) ()
recordBid' = undefined

readBids' :: AuctionId -> ReaderT a (EitherT Text IO) [(UTCTime, Bid)]
readBids' = undefined

createUser' :: User -> ReaderT a (EitherT Text IO) UserId
createUser' = undefined

parseRow :: Row Value -> Maybe LogEntry
parseRow row = do
  a <- lookup "btcAddr" row >>= valueAddr
  t <- lookup "eventTime" row >>= valueTime
  ev <- lookup "event" row >>= (valueEvent t)
  return $ LogEntry a ev

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
eventTable = Table "workEvents" [ Column "btcAddr"   (SQLVarChar 256) []
                                , Column "event"     (SQLVarChar 64) []
                                , Column "eventTime" (SQLDateTime DATETIME) [] 
                                ] []

auctionTable :: SQLTable
auctionTable = Table "auctions" [ Column "raiseAmouont" (SQLInt BIG False False) []
                                , Column "endsAt"       (SQLDateTime DATETIME) [] 
                                ] []

