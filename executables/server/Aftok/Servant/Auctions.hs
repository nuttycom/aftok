{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Auctions
  ( -- * API Types (re-exported from aftok-api)
    AuctionsAPI,
    ProtectedAuctionsAPI,
    ProjectAuctionsAPI,
    AuctionCreateRequest (..),
    BidCreateRequest (..),

    -- * Handlers
    protectedAuctionsServer,
    projectAuctionsServer,

    -- * JSON helpers
    auctionJSON,
    bidIdJSON,
  )
where

import Aftok.API.Auctions
  ( AuctionCreateRequest (..),
    AuctionsAPI,
    BidCreateRequest (..),
    ProjectAuctionsAPI,
    ProtectedAuctionsAPI,
  )
import Aftok.Auction
  ( Auction (Auction),
    AuctionId,
    Bid (Bid),
    BidId,
    auctionEnd,
    auctionStart,
    description,
    initiator,
    name,
    projectId,
    raiseAmount,
    _BidId,
  )
import Aftok.Currency (Amount)
import Aftok.Database
  ( Limit (..),
    createAuction,
    createBid,
    findAuction,
    listAuctions,
  )
import Aftok.Interval (RangeQuery (..))
import Aftok.Json (amountJSON, idValue, obj, parseAmountJSON, v1)
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Types (ProjectId, _ProjectId, _UserId)
import Aftok.Util (fromMaybeT)
import Control.Lens (to, (^.))
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson (Value, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)
import Data.Hourglass.Types (Seconds (..))
import qualified Data.Thyme.Clock as C
import Servant
import Servant.Auth.Server (AuthResult (..))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Protected auctions server (for single auction operations)
protectedAuctionsServer ::
  AuthResult AuthenticatedUser ->
  AuctionId ->
  ServerT
    ( Get '[JSON] Value
        :<|> "bid" :> ReqBody '[JSON] BidCreateRequest :> Post '[JSON] Value
    )
    AppM
protectedAuctionsServer authResult aid =
  auctionGetHandler authResult aid
    :<|> auctionBidHandler authResult aid

-- | Project auctions server (for listing and creating auctions)
projectAuctionsServer ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT ProjectAuctionsAPI AppM
projectAuctionsServer authResult pid =
  auctionListHandler authResult pid
    :<|> auctionCreateHandler authResult pid

-- | Get a specific auction
auctionGetHandler ::
  AuthResult AuthenticatedUser ->
  AuctionId ->
  AppM Value
auctionGetHandler (Authenticated user) aid = do
  let uid = auUserId user
  auction <-
    fromMaybeT
      (throwError err404 {errBody = "Auction not found"})
      (mapMaybeT runDB $ findAuction aid uid)
  pure $ auctionJSON auction
auctionGetHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a bid on an auction
auctionBidHandler ::
  AuthResult AuthenticatedUser ->
  AuctionId ->
  BidCreateRequest ->
  AppM Value
auctionBidHandler (Authenticated user) aid req = do
  let uid = auUserId user
  timestamp <- liftIO C.getCurrentTime
  amount <- case parseEither parseAmountJSON (bcrBidAmount req) of
    Left err -> throwError err400 {errBody = "Invalid bid amount: " <> encodeUtf8 (toText err)}
    Right a -> pure a
  let bid = Bid uid (Seconds $ fromIntegral $ bcrBidSeconds req) amount timestamp
  bidId <- runDB $ createBid aid uid bid
  pure $ bidIdJSON bidId
auctionBidHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | List auctions for a project
auctionListHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AppM Value
auctionListHandler (Authenticated user) pid = do
  let uid = auUserId user
      rangeQuery = Always
      limit = Limit 100 -- reasonable default
  auctions <- runDB $ listAuctions uid pid rangeQuery limit
  pure $ A.toJSON $ fmap auctionJSON auctions
auctionListHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a new auction
auctionCreateHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AuctionCreateRequest ->
  AppM AuctionId
auctionCreateHandler (Authenticated user) pid req = do
  let uid = auUserId user
  now <- liftIO C.getCurrentTime
  amount <- case parseEither parseAmountJSON (acrRaiseAmount req) of
    Left err -> throwError err400 {errBody = "Invalid raise amount: " <> encodeUtf8 (toText err)}
    Right a -> pure a
  runDB $
    createAuction $
      Auction
        pid
        uid
        now
        (acrName req)
        (acrDescription req)
        amount
        (acrAuctionStart req)
        (acrAuctionEnd req)
auctionCreateHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- JSON Serializers
--------------------------------------------------------------------------------

-- | Serialize an auction to JSON
auctionJSON :: Auction Amount -> Value
auctionJSON x =
  v1 $
    obj
      [ "projectId" .= idValue (projectId . _ProjectId) x,
        "initiator" .= idValue (initiator . _UserId) x,
        "name" .= (x ^. name),
        "description" .= (x ^. description),
        "raiseAmount" .= (x ^. (raiseAmount . to amountJSON)),
        "auctionStart" .= (x ^. auctionStart),
        "auctionEnd" .= (x ^. auctionEnd)
      ]

-- | Serialize a bid ID to JSON
bidIdJSON :: BidId -> Value
bidIdJSON pid = v1 $ obj ["bidId" .= (pid ^. _BidId)]
