{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Auctions
  ( auctionCreateHandler
  , auctionGetHandler
  ) where

import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Thyme.Clock   as C

import           Aftok.Auction      (AuctionId, Auction(..))
import           Aftok.Database     (createAuction, findAuction)
import           Aftok.Json
import           Aftok.Types

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import           Snap

data AuctionCreateRequest = CA { raiseAmount :: Word64, auctionEnd :: C.UTCTime }

auctionCreateParser :: Value -> Parser AuctionCreateRequest
auctionCreateParser = unv1 "auctions" $ \v ->
  case v of
    (Object o) -> CA <$> o .: "raiseAmount"
                     <*> o .: "auctionEnd"
    _          -> mzero

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
  maybeAuc <- snapEval $ findAuction aid uid
  maybe (snapError 404 $ "Auction not found for id " <> tshow aid) pure maybeAuc
