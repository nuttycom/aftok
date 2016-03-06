{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Auctions
  ( auctionCreateHandler
  ) where

import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Thyme.Clock   as C
--import           Data.Thyme.Format.Aeson ()

import           Aftok.Database     (createAuction)
import           Aftok.Auction      (AuctionId, Auction(..))
import           Aftok.Json
import           Aftok.Types

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import           Snap.Snaplet

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
  snapEval . createAuction pid $ Auction uid (Satoshi . raiseAmount $ req) (auctionEnd req)
