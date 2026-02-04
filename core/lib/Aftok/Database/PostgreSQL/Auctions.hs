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
    auctionEnd,
    auctionStart,
    bidAmount,
    bidSeconds,
    bidTime,
    bidUser,
    description,
    initiator,
    name,
    projectId,
    raiseAmount,
    _AuctionId,
  )
import Aftok.Currency (Amount (..))
import Aftok.Database (Limit (..))
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    currencyAmountParser,
    currencyType,
    currencyValue,
    idParser,
    pinsert,
    pquery,
    utcParser,
  )
import Aftok.Interval (RangeQuery (..))
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

auctionParser :: RowParser (Auction Amount)
auctionParser =
  Auction
    <$> idParser ProjectId
    <*> idParser UserId
    <*> utcParser
    <*> field
    <*> field
    <*> currencyAmountParser
    <*> utcParser
    <*> utcParser

bidParser :: RowParser (Bid Amount)
bidParser =
  Bid <$> idParser UserId <*> (Seconds <$> field) <*> currencyAmountParser <*> utcParser

createAuction :: Auction Amount -> DBM AuctionId
createAuction auc =
  pinsert
    AuctionId
    [sql| INSERT INTO auctions (project_id, initiator_id, name, description, currency, raise_amount, start_time, end_time)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id |]
    ( auc ^. projectId . _ProjectId,
      auc ^. initiator . _UserId,
      auc ^. name,
      auc ^. description,
      auc ^. raiseAmount . to currencyType,
      auc ^. raiseAmount . to currencyValue,
      auc ^. auctionStart . to C.fromThyme,
      auc ^. auctionEnd . to C.fromThyme
    )

findAuction :: AuctionId -> DBM (Maybe (Auction Amount))
findAuction aucId =
  headMay
    <$> pquery
      auctionParser
      [sql| SELECT project_id, initiator_id, created_at, name, description, raise_amount, start_time, end_time
          FROM auctions
          WHERE id = ? |]
      (Only (aucId ^. _AuctionId))

listAuctions :: ProjectId -> RangeQuery -> Limit -> DBM [Auction Amount]
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

createBid :: AuctionId -> Bid Amount -> DBM BidId
createBid (AuctionId aucId) bid =
  pinsert
    BidId
    [sql| INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time)
          VALUES (?, ?, ?, ?, ?) RETURNING id |]
    ( aucId,
      bid ^. (bidUser . _UserId),
      case bid ^. bidSeconds of
        (Seconds i) -> i,
      bid ^. bidAmount . to currencyType,
      bid ^. bidAmount . to currencyValue,
      bid ^. (bidTime . to C.fromThyme)
    )

findBids :: AuctionId -> DBM [(BidId, Bid Amount)]
findBids aucId =
  pquery
    ((,) <$> idParser BidId <*> bidParser)
    [sql| SELECT id, bidder_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ? |]
    (Only (aucId ^. _AuctionId))
