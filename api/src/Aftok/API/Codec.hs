{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Autodocodec 'HasCodec' instances for core library types that appear
-- in API request/response types. These codecs define the API wire format
-- only — the database layer has its own serialization and is unaffected.
module Aftok.API.Codec () where

import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), Recurrence (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..), PaymentRequestId (..))
import Aftok.TimeLog (AmendmentId (..), EventId (..), LogEvent (..))
import Aftok.Types
  ( AccountId (..),
    CreditTo (..),
    DepreciationFunction (..),
    ProjectId (..),
    UserId (..),
    UserName (..),
  )
import Autodocodec
  ( HasCodec (..),
    JSONCodec,
    bimapCodec,
    dimapCodec,
    disjointEitherCodec,
    object,
    optionalField,
    requiredField',
    (.=),
    (<?>),
  )
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme, toThyme)
import qualified Data.Time as Time
import qualified Data.UUID as UUID

--------------------------------------------------------------------------------
-- UUID wrapper types
--------------------------------------------------------------------------------

uuidCodec :: (a -> UUID.UUID) -> (UUID.UUID -> a) -> Text -> JSONCodec a
uuidCodec unwrap wrap name =
  bimapCodec
    (maybe (Left "Invalid UUID") Right . fmap wrap . UUID.fromText)
    (UUID.toText . unwrap)
    (codec @Text <?> name)

instance HasCodec UserId where
  codec = uuidCodec (\(UserId u) -> u) UserId "UserId UUID"

instance HasCodec ProjectId where
  codec = uuidCodec (\(ProjectId u) -> u) ProjectId "ProjectId UUID"

instance HasCodec AccountId where
  codec = uuidCodec (\(AccountId u) -> u) AccountId "AccountId UUID"

instance HasCodec EventId where
  codec = uuidCodec (\(EventId u) -> u) EventId "EventId UUID"

instance HasCodec AmendmentId where
  codec = uuidCodec (\(AmendmentId u) -> u) AmendmentId "AmendmentId UUID"

instance HasCodec BillableId where
  codec = uuidCodec (\(BillableId u) -> u) BillableId "BillableId UUID"

instance HasCodec AuctionId where
  codec = uuidCodec (\(AuctionId u) -> u) AuctionId "AuctionId UUID"

instance HasCodec SubscriptionId where
  codec = uuidCodec (\(SubscriptionId u) -> u) SubscriptionId "SubscriptionId UUID"

instance HasCodec PaymentRequestId where
  codec = uuidCodec (\(PaymentRequestId u) -> u) PaymentRequestId "PaymentRequestId UUID"

instance HasCodec PaymentId where
  codec = uuidCodec (\(PaymentId u) -> u) PaymentId "PaymentId UUID"

--------------------------------------------------------------------------------
-- Thyme UTCTime
--------------------------------------------------------------------------------

instance HasCodec C.UTCTime where
  codec = dimapCodec toThyme fromThyme (codec @Time.UTCTime)

--------------------------------------------------------------------------------
-- Simple newtypes
--------------------------------------------------------------------------------

instance HasCodec UserName where
  codec = dimapCodec UserName (\(UserName t) -> t) codec

--------------------------------------------------------------------------------
-- CreditTo
--
-- Matches existing creditToJSON/parseCreditToV2 format:
--   {"creditToAccount": "<uuid>"}

-- | {"creditToUser": "<uuid>"}
-- | {"creditToProject": "<uuid>"}

--------------------------------------------------------------------------------

instance HasCodec CreditTo where
  codec =
    dimapCodec fromEither toEither $
      disjointEitherCodec accountCodec $
        disjointEitherCodec userCodec projectCodec
    where
      accountCodec =
        object "CreditToAccount" $
          requiredField' "creditToAccount" .= id
      userCodec =
        object "CreditToUser" $
          requiredField' "creditToUser" .= id
      projectCodec =
        object "CreditToProject" $
          requiredField' "creditToProject" .= id
      fromEither :: Either AccountId (Either UserId ProjectId) -> CreditTo
      fromEither = \case
        Left a -> CreditToAccount a
        Right (Left u) -> CreditToUser u
        Right (Right p) -> CreditToProject p
      toEither :: CreditTo -> Either AccountId (Either UserId ProjectId)
      toEither = \case
        CreditToAccount a -> Left a
        CreditToUser u -> Right (Left u)
        CreditToProject p -> Right (Right p)

--------------------------------------------------------------------------------
-- DepreciationFunction
--
-- Matches existing depfToJSON/depfFromJSON format:
--   {"type": "LinearDepreciation", "arguments": {"undep": N, "dep": N}}
--------------------------------------------------------------------------------

-- Helper type for the nested "arguments" object
data LinearDepArgs = LinearDepArgs Int Int

instance HasCodec LinearDepArgs where
  codec =
    object "LinearDepreciationArgs" $
      LinearDepArgs
        <$> requiredField' "undep" .= (\(LinearDepArgs u _) -> u)
        <*> requiredField' "dep" .= (\(LinearDepArgs _ d) -> d)

instance HasCodec DepreciationFunction where
  codec =
    object "DepreciationFunction" $
      (\_ args -> case args of LinearDepArgs u d -> LinearDepreciation u d)
        <$> requiredField' "type" .= (\(LinearDepreciation _ _) -> ("LinearDepreciation" :: Text))
        <*> requiredField' "arguments" .= (\(LinearDepreciation u d) -> LinearDepArgs u d)

--------------------------------------------------------------------------------
-- LogEvent
--
-- Matches existing logEventToJSON format:
--   {"start": {"eventTime": "<iso8601>"}}

-- | {"stop": {"eventTime": "<iso8601>"}}

--------------------------------------------------------------------------------

newtype EventTimeWrapper = EventTimeWrapper C.UTCTime

instance HasCodec EventTimeWrapper where
  codec =
    object "EventTime" $
      EventTimeWrapper <$> requiredField' "eventTime" .= (\(EventTimeWrapper t) -> t)

instance HasCodec LogEvent where
  codec =
    dimapCodec fromEither toEither $
      disjointEitherCodec startCodec stopCodec
    where
      startCodec =
        object "StartWork" $
          requiredField' "start" .= id
      stopCodec =
        object "StopWork" $
          requiredField' "stop" .= id
      fromEither :: Either EventTimeWrapper EventTimeWrapper -> LogEvent
      fromEither = \case
        Left (EventTimeWrapper t) -> StartWork t
        Right (EventTimeWrapper t) -> StopWork t
      toEither :: LogEvent -> Either EventTimeWrapper EventTimeWrapper
      toEither = \case
        StartWork t -> Left (EventTimeWrapper t)
        StopWork t -> Right (EventTimeWrapper t)

--------------------------------------------------------------------------------
-- Recurrence
--
-- Matches existing recurrenceToJSON/parseRecurrence format:
--   {"annually": null} | {"monthly": N} | {"weekly": N} | {"onetime": null}
--------------------------------------------------------------------------------

instance HasCodec Recurrence where
  codec =
    dimapCodec fromEither toEither $
      disjointEitherCodec annuallyCodec $
        disjointEitherCodec monthlyCodec $
          disjointEitherCodec weeklyCodec onetimeCodec
    where
      annuallyCodec =
        object "Annually" $
          optionalField "annually" "null marker" .= id
      monthlyCodec =
        object "Monthly" $
          requiredField' "monthly" .= id
      weeklyCodec =
        object "Weekly" $
          requiredField' "weekly" .= id
      onetimeCodec =
        object "OneTime" $
          optionalField "onetime" "null marker" .= id
      fromEither :: Either (Maybe Int) (Either Int (Either Int (Maybe Int))) -> Recurrence
      fromEither = \case
        Left _ -> Annually
        Right (Left n) -> Monthly n
        Right (Right (Left n)) -> Weekly n
        Right (Right (Right _)) -> OneTime
      toEither :: Recurrence -> Either (Maybe Int) (Either Int (Either Int (Maybe Int)))
      toEither = \case
        Annually -> Left Nothing
        Monthly n -> Right (Left n)
        Weekly n -> Right (Right (Left n))
        OneTime -> Right (Right (Right Nothing))
