{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Auctions API types for the Aftok API.
module Aftok.API.Auctions
  ( -- * API Types
    AuctionsAPI,
    ProtectedAuctionsAPI,
    ProjectAuctionsAPI,

    -- * Request Types
    AuctionCreateRequest (..),
    AuctionCreateResponse (..),
    BidCreateRequest (..),
  )
where

import Aftok.API.Codec ()
import Aftok.API.Types ()
import Aftok.Auction (AuctionId)
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (toJSONViaCodec)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
    (.:),
    (.:?),
  )
import qualified Data.Aeson as A
import qualified Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Auction creation response
data AuctionCreateResponse = AuctionCreateResponse
  { acrAuctionId :: AuctionId
  }
  deriving (Generic)

instance HasCodec AuctionCreateResponse where
  codec =
    object "AuctionCreateResponse" $
      AuctionCreateResponse
        <$> requiredField' "auctionId" AC..= acrAuctionId

instance ToJSON AuctionCreateResponse where toJSON = toJSONViaCodec

-- | Auction creation request
data AuctionCreateRequest = AuctionCreateRequest
  { acrName :: Text,
    acrDescription :: Maybe Text,
    acrRaiseAmount :: Value, -- JSON value to be parsed with parseAmountJSON
    acrAuctionStart :: C.UTCTime,
    acrAuctionEnd :: C.UTCTime
  }

instance FromJSON AuctionCreateRequest where
  parseJSON = A.withObject "AuctionCreateRequest" $ \o -> do
    auctions <- o .: "auctions"
    AuctionCreateRequest
      <$> auctions .: "auctionName"
      <*> auctions .:? "auctionDesc"
      <*> auctions .: "raiseAmount"
      <*> auctions .: "auctionStart"
      <*> auctions .: "auctionEnd"

-- | The inner "auctions" object for AuctionCreateRequest codec
data AuctionInner = AuctionInner
  { aiName :: Text,
    aiDescription :: Maybe Text,
    aiRaiseAmount :: Value,
    aiStart :: C.UTCTime,
    aiEnd :: C.UTCTime
  }

instance HasCodec AuctionInner where
  codec =
    object "AuctionInner" $
      AuctionInner
        <$> requiredField' "auctionName" AC..= aiName
        <*> optionalField' "auctionDesc" AC..= aiDescription
        <*> requiredField' "raiseAmount" AC..= aiRaiseAmount
        <*> requiredField' "auctionStart" AC..= aiStart
        <*> requiredField' "auctionEnd" AC..= aiEnd

instance HasCodec AuctionCreateRequest where
  codec =
    object "AuctionCreateRequest" $
      (\inner -> AuctionCreateRequest (aiName inner) (aiDescription inner) (aiRaiseAmount inner) (aiStart inner) (aiEnd inner))
        <$> requiredField' "auctions" AC..= (\r -> AuctionInner (acrName r) (acrDescription r) (acrRaiseAmount r) (acrAuctionStart r) (acrAuctionEnd r))

-- | Bid creation request
data BidCreateRequest = BidCreateRequest
  { bcrBidSeconds :: Int,
    bcrBidAmount :: Value -- JSON value to be parsed with parseAmountJSON
  }

instance FromJSON BidCreateRequest where
  parseJSON = A.withObject "BidCreateRequest" $ \o -> do
    bids <- o .: "bids"
    BidCreateRequest
      <$> bids .: "bidSeconds"
      <*> bids .: "bidAmount"

-- | The inner "bids" object for BidCreateRequest codec
data BidInner = BidInner
  { biSeconds :: Int,
    biAmount :: Value
  }

instance HasCodec BidInner where
  codec =
    object "BidInner" $
      BidInner
        <$> requiredField' "bidSeconds" AC..= biSeconds
        <*> requiredField' "bidAmount" AC..= biAmount

instance HasCodec BidCreateRequest where
  codec =
    object "BidCreateRequest" $
      (\inner -> BidCreateRequest (biSeconds inner) (biAmount inner))
        <$> requiredField' "bids" AC..= (\r -> BidInner (bcrBidSeconds r) (bcrBidAmount r))

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public Auctions API (none currently)
type AuctionsAPI = EmptyAPI

-- | Protected auctions API for single auction operations
type ProtectedAuctionsAPI =
  "auctions"
    :> Capture "auctionId" AuctionId
    :> ( -- GET /auctions/:auctionId
         Get '[JSON] Value
           -- POST /auctions/:auctionId/bid
           :<|> "bid" :> ReqBody '[JSON] BidCreateRequest :> Post '[JSON] Value
       )

-- | Project-specific auctions API (nested under projects)
type ProjectAuctionsAPI =
  -- GET /projects/:projectId/auctions
  Get '[JSON] Value
    -- POST /projects/:projectId/auctions
    :<|> ReqBody '[JSON] AuctionCreateRequest :> Post '[JSON] AuctionCreateResponse
