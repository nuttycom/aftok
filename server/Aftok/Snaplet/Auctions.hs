{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Auctions
  ( auctionCreateHandler
  , auctionGetHandler
  , auctionBidHandler
  ) where

import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hourglass.Types (Seconds(..))
import           Data.Thyme.Clock   as C

import           Aftok              (UserId)
import           Aftok.Auction      (AuctionId, Auction(..), BidId, Bid(..))
import           Aftok.Database     (createAuction, findAuction, createBid)
import           Aftok.Json
import           Aftok.Types

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import           Snap

data AuctionCreateRequest = CA { raiseAmount :: Word64, auctionEnd :: C.UTCTime }

auctionCreateParser :: Value -> Parser AuctionCreateRequest
auctionCreateParser = unv1 "auctions" p where
  p o = CA <$> o .: "raiseAmount" <*> o .: "auctionEnd"

bidCreateParser :: UserId -> C.UTCTime-> Value -> Parser Bid
bidCreateParser uid t = unv1 "bids" p where
  p o = Bid uid <$> (Seconds <$> o .: "bidSeconds")
                <*> (Satoshi <$> o .: "bidAmount")
                <*> pure t

auctionCreateHandler :: Handler App App AuctionId
auctionCreateHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestJSON 4096
  req <- either (snapError 400 . tshow) pure $ parseEither auctionCreateParser requestBody
  --t <- liftIO C.getCurrentTime
  snapEval . createAuction $ Auction pid uid (Satoshi . raiseAmount $ req) (auctionEnd req)

auctionGetHandler :: Handler App App Auction
auctionGetHandler = do
  uid <- requireUserId
  aid <- requireAuctionId
  maybeAuc <- snapEval $ findAuction aid uid -- this will verify auction access
  maybe (snapError 404 $ "Auction not found for id " <> tshow aid) pure maybeAuc

auctionBidHandler :: Handler App App BidId
auctionBidHandler = do
  uid <- requireUserId
  aid <- requireAuctionId
  timestamp <- liftIO C.getCurrentTime
  requestBody <- readRequestJSON 4096
  bid <- either (snapError 400 . tshow) pure $ parseEither (bidCreateParser uid timestamp) requestBody
  snapEval $ createBid aid uid bid
