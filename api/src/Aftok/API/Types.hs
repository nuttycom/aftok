{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Core type instances for the Aftok API.
-- This module provides FromHttpApiData, ToHttpApiData, FromJSON, and ToJSON
-- instances for ID types used in URL parameters and JSON responses.
module Aftok.API.Types () where

import Aftok.API.Codec ()
import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..))
import Aftok.Types (GitHubRepoLinkId (..), GitHubUsername (..), GitHubWebhookEventId (..), ProjectId (..), UserId (..))
import Autodocodec.Aeson (toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

--------------------------------------------------------------------------------
-- FromHttpApiData / ToHttpApiData instances for URL parameters
--------------------------------------------------------------------------------

instance FromHttpApiData ProjectId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid ProjectId: " <> t
    Just u -> Right $ ProjectId u

instance ToHttpApiData ProjectId where
  toUrlPiece (ProjectId u) = UUID.toText u

instance FromHttpApiData BillableId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid BillableId: " <> t
    Just u -> Right $ BillableId u

instance ToHttpApiData BillableId where
  toUrlPiece (BillableId u) = UUID.toText u

instance FromHttpApiData AuctionId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid AuctionId: " <> t
    Just u -> Right $ AuctionId u

instance ToHttpApiData AuctionId where
  toUrlPiece (AuctionId u) = UUID.toText u

instance FromHttpApiData SubscriptionId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid SubscriptionId: " <> t
    Just u -> Right $ SubscriptionId u

instance ToHttpApiData SubscriptionId where
  toUrlPiece (SubscriptionId u) = UUID.toText u

-- | FromHttpApiData instance for thyme's UTCTime
-- Convert via the standard time library UTCTime which has an instance
instance FromHttpApiData C.UTCTime where
  parseUrlPiece t = toThyme <$> (parseUrlPiece t :: Either Text Time.UTCTime)

instance ToHttpApiData C.UTCTime where
  toUrlPiece = toUrlPiece . fromThyme

--------------------------------------------------------------------------------
-- JSON instances for ID types (codec-derived)
--------------------------------------------------------------------------------

instance ToJSON UserId where toJSON = toJSONViaCodec
instance FromJSON UserId where parseJSON = parseJSONViaCodec

instance ToJSON ProjectId where toJSON = toJSONViaCodec
instance FromJSON ProjectId where parseJSON = parseJSONViaCodec

instance ToJSON BillableId where toJSON = toJSONViaCodec
instance FromJSON BillableId where parseJSON = parseJSONViaCodec

instance ToJSON SubscriptionId where toJSON = toJSONViaCodec
instance FromJSON SubscriptionId where parseJSON = parseJSONViaCodec

instance ToJSON AuctionId where toJSON = toJSONViaCodec
instance FromJSON AuctionId where parseJSON = parseJSONViaCodec

instance ToJSON PaymentId where toJSON = toJSONViaCodec
instance FromJSON PaymentId where parseJSON = parseJSONViaCodec

instance ToJSON GitHubRepoLinkId where toJSON = toJSONViaCodec
instance FromJSON GitHubRepoLinkId where parseJSON = parseJSONViaCodec

instance ToJSON GitHubWebhookEventId where toJSON = toJSONViaCodec
instance FromJSON GitHubWebhookEventId where parseJSON = parseJSONViaCodec

instance ToJSON GitHubUsername where toJSON = toJSONViaCodec
instance FromJSON GitHubUsername where parseJSON = parseJSONViaCodec
