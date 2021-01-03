{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.Auctions
  ( createAuction,
    listAuctions,
    findAuction,
    createBid,
    findBids,
  )
where

import Aftok.Auction
  ( Auction (..),
    AuctionId (..),
    Bid (..),
    BidId (..),
    _AuctionId,
    auctionEnd,
    bidAmount,
    bidSeconds,
    bidTime,
    bidUser,
    initiator,
    projectId,
    raiseAmount,
    name,
    description
  )
-- import           Aftok.Currency                 ( Amount(..) )

-- import qualified Aftok.Currency.Bitcoin        as Bitcoin
import Aftok.Currency.Bitcoin (_Satoshi)
-- import qualified Aftok.Currency.Zcash          as Zcash

import Aftok.Database (Limit(..))
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    btcAmountParser,
    idParser,
    pinsert,
    pquery,
    utcParser,
  )
import Aftok.Interval (RangeQuery(..))
import Aftok.Types
  ( ProjectId (..),
    UserId (..),
    _ProjectId,
    _UserId,
  )
import Control.Lens
import Data.Hourglass (Seconds (..))
import qualified Data.Thyme.Time as C
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField ()
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Safe (headMay)
import Prelude hiding (null)

auctionParser :: RowParser Auction
auctionParser =
  Auction
    <$> idParser ProjectId
    <*> idParser UserId
    <*> utcParser
    <*> field
    <*> field
    <*> btcAmountParser
    <*> utcParser
    <*> utcParser

bidParser :: RowParser Bid
bidParser =
  Bid <$> idParser UserId <*> (Seconds <$> field) <*> btcAmountParser <*> utcParser

createAuction :: Auction -> DBM AuctionId
createAuction auc =
  pinsert
    AuctionId
    [sql| INSERT INTO auctions (project_id, initiator_id, name, description, raise_amount, end_time)
          VALUES (?, ?, ?, ?) RETURNING id |]
    ( auc ^. (projectId . _ProjectId),
      auc ^. (initiator . _UserId),
      auc ^. name,
      auc ^. description,
      auc ^. (raiseAmount . _Satoshi),
      auc ^. (auctionEnd . to C.fromThyme)
    )

findAuction :: AuctionId -> DBM (Maybe Auction)
findAuction aucId =
  headMay
    <$> pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE id = ? |]
      (Only (aucId ^. _AuctionId))

listAuctions :: ProjectId -> RangeQuery -> Limit -> DBM [Auction]
listAuctions pid rq (Limit limit) =
  case rq of
    (Before e) ->
      pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE project_id = ?
          AND end_time < ?
          LIMIT ? |]
      (pid ^. _ProjectId, C.fromThyme e, limit)
    (During s e) ->
      pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE project_id = ?
          AND end_time >= ? AND end_time < ?
          LIMIT ? |]
      (pid ^. _ProjectId, C.fromThyme s, C.fromThyme e, limit)
    (After s) ->
      pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE project_id = ?
          AND end_time >= ?
          LIMIT ? |]
      (pid ^. _ProjectId, C.fromThyme s, limit)
    Always ->
      pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE project_id = ?
          LIMIT ? |]
      (pid ^. _ProjectId, limit)

createBid :: AuctionId -> Bid -> DBM BidId
createBid (AuctionId aucId) bid =
  pinsert
    BidId
    [sql| INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time)
          VALUES (?, ?, ?, ?, ?) RETURNING id |]
    ( aucId,
      bid ^. (bidUser . _UserId),
      case bid ^. bidSeconds of
        (Seconds i) -> i,
      bid ^. (bidAmount . _Satoshi),
      bid ^. (bidTime . to C.fromThyme)
    )

findBids :: AuctionId -> DBM [(BidId, Bid)]
findBids aucId =
  pquery
    ((,) <$> idParser BidId <*> bidParser)
    [sql| SELECT id, bidder_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ? |]
    (Only (aucId ^. _AuctionId))
