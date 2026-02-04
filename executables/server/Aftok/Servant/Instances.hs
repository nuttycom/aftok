{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aftok.Servant.Instances () where

import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..))
import Aftok.Types (ProjectId (..), UserId (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Servant (FromHttpApiData (..), ToHttpApiData (..))

-- | FromHttpApiData instance for ProjectId
instance FromHttpApiData ProjectId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid ProjectId: " <> t
    Just u -> Right $ ProjectId u

instance ToHttpApiData ProjectId where
  toUrlPiece (ProjectId u) = UUID.toText u

-- | FromHttpApiData instance for BillableId
instance FromHttpApiData BillableId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid BillableId: " <> t
    Just u -> Right $ BillableId u

instance ToHttpApiData BillableId where
  toUrlPiece (BillableId u) = UUID.toText u

-- | FromHttpApiData instance for AuctionId
instance FromHttpApiData AuctionId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid AuctionId: " <> t
    Just u -> Right $ AuctionId u

instance ToHttpApiData AuctionId where
  toUrlPiece (AuctionId u) = UUID.toText u

-- | FromHttpApiData instance for SubscriptionId
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

-- | JSON instances for ID types used in API responses

instance ToJSON UserId where
  toJSON (UserId u) = toJSON $ UUID.toText u

instance FromJSON UserId where
  parseJSON = withText "UserId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for UserId"
      Just u -> pure $ UserId u

instance ToJSON ProjectId where
  toJSON (ProjectId u) = toJSON $ UUID.toText u

instance FromJSON ProjectId where
  parseJSON = withText "ProjectId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for ProjectId"
      Just u -> pure $ ProjectId u

instance ToJSON BillableId where
  toJSON (BillableId u) = toJSON $ UUID.toText u

instance FromJSON BillableId where
  parseJSON = withText "BillableId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for BillableId"
      Just u -> pure $ BillableId u

instance ToJSON SubscriptionId where
  toJSON (SubscriptionId u) = toJSON $ UUID.toText u

instance FromJSON SubscriptionId where
  parseJSON = withText "SubscriptionId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for SubscriptionId"
      Just u -> pure $ SubscriptionId u

instance ToJSON AuctionId where
  toJSON (AuctionId u) = toJSON $ UUID.toText u

instance FromJSON AuctionId where
  parseJSON = withText "AuctionId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for AuctionId"
      Just u -> pure $ AuctionId u

instance ToJSON PaymentId where
  toJSON (PaymentId u) = toJSON $ UUID.toText u

instance FromJSON PaymentId where
  parseJSON = withText "PaymentId" $ \t ->
    case UUID.fromText t of
      Nothing -> fail "Invalid UUID for PaymentId"
      Just u -> pure $ PaymentId u
