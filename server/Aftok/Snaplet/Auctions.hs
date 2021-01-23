{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Auctions
  ( auctionCreateHandler,
    auctionGetHandler,
    auctionBidHandler,
    auctionListHandler,
    auctionJSON,
    bidIdJSON,
  )
where

import Aftok.Auction
  ( Auction (Auction),
    AuctionId,
    Bid (Bid),
    BidId,
    _BidId,
    auctionEnd,
    auctionStart,
    description,
    initiator,
    name,
    projectId,
    raiseAmount,
  )
import Aftok.Currency (Amount)
import Aftok.Database
  ( Limit (..),
    createAuction,
    createBid,
    findAuction,
    listAuctions,
  )
import Aftok.Json
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Util (decimalParam, rangeQueryParam)
import Aftok.Types (UserId, _ProjectId, _UserId)
import Aftok.Util (fromMaybeT)
import Control.Lens ((^.), to)
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson
import Data.Aeson.Types
import Data.Hourglass.Types (Seconds (..))
import Data.Thyme.Clock as C
import Snap.Snaplet as S

data AuctionCreateRequest
  = CA
      { acrName :: Text,
        acrDescription :: Maybe Text,
        acrRaiseAmount :: Amount,
        acrAuctionStart :: C.UTCTime,
        acrAuctionEnd :: C.UTCTime
      }

auctionCreateParser :: Value -> Parser AuctionCreateRequest
auctionCreateParser = unv1 "auctions" p
  where
    p o =
      CA
        <$> o .: "auctionName"
        <*> o .:? "auctionDesc"
        <*> (parseAmountJSON =<< o .: "raiseAmount")
        <*> o .: "auctionStart"
        <*> o .: "auctionEnd"

bidCreateParser :: UserId -> C.UTCTime -> Value -> Parser (Bid Amount)
bidCreateParser uid t = unv1 "bids" p
  where
    p o =
      Bid uid
        <$> (Seconds <$> o .: "bidSeconds")
        <*> (parseAmountJSON =<< o .: "bidAmount")
        <*> pure t

auctionCreateHandler :: S.Handler App App AuctionId
auctionCreateHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestJSON 4096
  req <-
    either (snapError 400 . show) pure $
      parseEither auctionCreateParser requestBody
  now <- liftIO C.getCurrentTime
  snapEval . createAuction $
    Auction
      pid
      uid
      now
      (acrName req)
      (acrDescription req)
      (acrRaiseAmount $ req)
      (acrAuctionStart req)
      (acrAuctionEnd req)

auctionListHandler :: S.Handler App App [Auction Amount]
auctionListHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  rq <- rangeQueryParam
  limit <- Limit . fromMaybe 1 <$> decimalParam "limit"
  snapEval $ listAuctions uid pid rq limit

auctionGetHandler :: S.Handler App App (Auction Amount)
auctionGetHandler = do
  uid <- requireUserId
  aid <- requireAuctionId
  fromMaybeT
    (snapError 404 $ "Auction not found for id " <> show aid)
    (mapMaybeT snapEval $ findAuction aid uid) -- this will verify auction access

auctionBidHandler :: S.Handler App App BidId
auctionBidHandler = do
  uid <- requireUserId
  aid <- requireAuctionId
  timestamp <- liftIO C.getCurrentTime
  requestBody <- readRequestJSON 4096
  bid <-
    either (snapError 400 . show) pure $
      parseEither (bidCreateParser uid timestamp) requestBody
  snapEval $ createBid aid uid bid

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

bidIdJSON :: BidId -> Value
bidIdJSON pid = v1 $ obj ["bidId" .= (pid ^. _BidId)]
