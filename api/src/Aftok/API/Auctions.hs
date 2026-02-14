{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

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

import Aftok.API.Types ()
import Aftok.Auction (AuctionId)
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
  { auctionId :: AuctionId
  }
  deriving (Generic)

instance ToJSON AuctionCreateResponse

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
