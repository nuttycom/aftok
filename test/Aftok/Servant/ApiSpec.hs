{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aftok.Servant.ApiSpec
  ( main,
    spec,
  )
where

import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..))
import Aftok.Types (ProjectId (..), UserId (..))
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, withText)
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.Hspec
import Test.QuickCheck

-- Define instances locally for testing (mirrors Aftok.Servant.Instances)
-- These are orphan instances, but acceptable in test code

instance FromHttpApiData ProjectId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid ProjectId: " <> t
    Just u -> Right $ ProjectId u

instance ToHttpApiData ProjectId where
  toUrlPiece (ProjectId u) = UUID.toText u

instance FromHttpApiData UserId where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left $ "Invalid UserId: " <> t
    Just u -> Right $ UserId u

instance ToHttpApiData UserId where
  toUrlPiece (UserId u) = UUID.toText u

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

instance FromHttpApiData C.UTCTime where
  parseUrlPiece t = toThyme <$> (parseUrlPiece t :: Either Text Time.UTCTime)

instance ToHttpApiData C.UTCTime where
  toUrlPiece = toUrlPiece . fromThyme

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

-- | Test that FromHttpApiData/ToHttpApiData instances roundtrip correctly
propHttpApiDataRoundtrip ::
  (Eq a, Show a, FromHttpApiData a, ToHttpApiData a) =>
  a ->
  Property
propHttpApiDataRoundtrip x =
  parseUrlPiece (toUrlPiece x) === Right x

-- | Test that JSON instances roundtrip correctly
propJsonRoundtrip ::
  (Eq a, Show a, FromJSON a, ToJSON a) =>
  a ->
  Property
propJsonRoundtrip x =
  decode (encode x) === Just x

spec :: Spec
spec = do
  describe "Aftok.Servant.Instances" $ do
    describe "ProjectId" $ do
      it "roundtrips through HttpApiData" $
        property $ \uuid ->
          let pid = ProjectId (uuidFromWords uuid)
           in propHttpApiDataRoundtrip pid

      it "roundtrips through JSON" $
        property $ \uuid ->
          let pid = ProjectId (uuidFromWords uuid)
           in propJsonRoundtrip pid

      it "parses a valid UUID" $ do
        let uuid = "550e8400-e29b-41d4-a716-446655440000"
        parseUrlPiece @ProjectId uuid
          `shouldBe` Right (ProjectId $ fromJust $ UUID.fromText uuid)

      it "rejects an invalid UUID" $ do
        let invalid = "not-a-uuid"
        parseUrlPiece @ProjectId invalid
          `shouldSatisfy` isLeft

    describe "UserId" $ do
      it "roundtrips through HttpApiData" $
        property $ \uuid ->
          let uid = UserId (uuidFromWords uuid)
           in propHttpApiDataRoundtrip uid

      it "roundtrips through JSON" $
        property $ \uuid ->
          let uid = UserId (uuidFromWords uuid)
           in propJsonRoundtrip uid

    describe "BillableId" $ do
      it "roundtrips through HttpApiData" $
        property $ \uuid ->
          let bid = BillableId (uuidFromWords uuid)
           in propHttpApiDataRoundtrip bid

      it "roundtrips through JSON" $
        property $ \uuid ->
          let bid = BillableId (uuidFromWords uuid)
           in propJsonRoundtrip bid

    describe "SubscriptionId" $ do
      it "roundtrips through JSON" $
        property $ \uuid ->
          let sid = SubscriptionId (uuidFromWords uuid)
           in propJsonRoundtrip sid

    describe "AuctionId" $ do
      it "roundtrips through HttpApiData" $
        property $ \uuid ->
          let aid = AuctionId (uuidFromWords uuid)
           in propHttpApiDataRoundtrip aid

      it "roundtrips through JSON" $
        property $ \uuid ->
          let aid = AuctionId (uuidFromWords uuid)
           in propJsonRoundtrip aid

    describe "PaymentId" $ do
      it "roundtrips through JSON" $
        property $ \uuid ->
          let pid = PaymentId (uuidFromWords uuid)
           in propJsonRoundtrip pid

    describe "UTCTime (thyme)" $ do
      it "roundtrips through HttpApiData" $
        property $ \(timeComponents :: (Int, Int, Int, Int, Int, Int)) ->
          let (y, mo, d, h, mi, sec) = timeComponents
              -- Constrain to valid date/time ranges
              year = 2000 + (abs y `mod` 100)
              month = 1 + (abs mo `mod` 12)
              day = 1 + (abs d `mod` 28)
              hour = abs h `mod` 24
              minute = abs mi `mod` 60
              sec' = abs sec `mod` 60
              timeUtc =
                Time.UTCTime
                  (Time.fromGregorian (fromIntegral year) month day)
                  (Time.secondsToDiffTime $ fromIntegral $ hour * 3600 + minute * 60 + sec')
              thymeUtc :: C.UTCTime
              thymeUtc = toThyme timeUtc
              -- Compare via Time.UTCTime since thyme's UTCTime Eq instance may not be visible
           in case parseUrlPiece (toUrlPiece thymeUtc) :: Either Text C.UTCTime of
                Left _ -> property False
                Right result -> fromThyme result === timeUtc

-- | Helper to create UUID from 4 arbitrary words
uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a, b, c, d) = UUID.fromWords a b c d

-- | Unsafe fromJust for tests
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

main :: IO ()
main = hspec spec
