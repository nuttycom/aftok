{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | OpenAPI 3 schema instances for all Aftok API types.
module Aftok.API.OpenApi () where

import qualified Data.Aeson as A
import Aftok.API.Auth (AuthenticatedUser (..), LoginRequest (..))
import Aftok.API.Billing
  ( BillableCreateRequest,
    BillableCreateResponse,
    BillableResponse,
    PaymentRequestCreateRequest,
    PaymentRequestResponse,
    SubscribeRequest,
    SubscribeResponse,
  )
import Aftok.API.Config (ClientConfig)
import Aftok.API.PasswordReset
  ( PasswordResetConfirm,
    PasswordResetRequest,
    PasswordResetResponse,
  )
import Aftok.API.Projects
  ( CommsAddress,
    ProjectCreateRequest,
    ProjectCreateResponse,
    ProjectDetailResponse,
    ProjectInviteRequest,
    ProjectInviteResponse,
    ProjectResponse,
    ProjectSummary,
  )
import Aftok.API.Session (LoginCheckResponse)
import Aftok.API.Users
  ( AccountSettingsResponse,
    RegisterRequest,
    RegisterResponse,
    SetPaymentAddressRequest,
    UsernameCheckResponse,
    ZAddrCheckResponse,
  )
import Aftok.API.WorkLog
  ( AmendEventResponse,
    EventAmendmentRequest,
    ExtendedLogEntryResponse,
    IntervalResponse,
    KeyedLogEntryResponse,
    LogEndRequest,
    LogStartRequest,
    WorkIndexEntry,
    WorkIndexResponse,
  )
import Aftok.API.Auctions
  ( AuctionCreateRequest,
    AuctionCreateResponse,
    BidCreateRequest,
  )
import Aftok.API.Payments (BIP70Data (..))
import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..))
import Aftok.Types (ProjectId (..), UserId (..))

import Control.Lens ((.~), (?~))
import Data.OpenApi
  ( NamedSchema (..),
    ToParamSchema (..),
    ToSchema (..),
  )
import qualified Data.OpenApi as OA
import qualified Data.Thyme.Clock as C
import qualified Data.Time as Time
import Servant.API ((:>))
import Servant.Auth (Auth)
import Servant.OpenApi (HasOpenApi (..))

--------------------------------------------------------------------------------
-- servant-auth Auth combinator support for OpenAPI
--------------------------------------------------------------------------------

-- | HasOpenApi instance for servant-auth's Auth combinator.
-- Strips the auth wrapper and documents it as requiring cookie/JWT auth.
instance (HasOpenApi api) => HasOpenApi (Auth auths a :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & OA.security .~ [OA.SecurityRequirement mempty]
      & OA.components . OA.securitySchemes .~ OA.SecurityDefinitions mempty

--------------------------------------------------------------------------------
-- ToParamSchema instances for ID types (used in URL captures)
--------------------------------------------------------------------------------

uuidParamSchema :: OA.Schema
uuidParamSchema = mempty
  & OA.type_ ?~ OA.OpenApiString
  & OA.format ?~ "uuid"

instance ToParamSchema ProjectId where
  toParamSchema _ = uuidParamSchema

instance ToParamSchema BillableId where
  toParamSchema _ = uuidParamSchema

instance ToParamSchema AuctionId where
  toParamSchema _ = uuidParamSchema

instance ToParamSchema SubscriptionId where
  toParamSchema _ = uuidParamSchema

-- | thyme UTCTime as query parameter (ISO 8601 string)
instance ToParamSchema C.UTCTime where
  toParamSchema _ = mempty
    & OA.type_ ?~ OA.OpenApiString
    & OA.format ?~ "date-time"

--------------------------------------------------------------------------------
-- ToSchema instances for core types
--------------------------------------------------------------------------------

instance ToSchema ProjectId where
  declareNamedSchema _ = pure $ NamedSchema (Just "ProjectId") uuidParamSchema

instance ToSchema UserId where
  declareNamedSchema _ = pure $ NamedSchema (Just "UserId") uuidParamSchema

instance ToSchema BillableId where
  declareNamedSchema _ = pure $ NamedSchema (Just "BillableId") uuidParamSchema

instance ToSchema AuctionId where
  declareNamedSchema _ = pure $ NamedSchema (Just "AuctionId") uuidParamSchema

instance ToSchema SubscriptionId where
  declareNamedSchema _ = pure $ NamedSchema (Just "SubscriptionId") uuidParamSchema

instance ToSchema PaymentId where
  declareNamedSchema _ = pure $ NamedSchema (Just "PaymentId") uuidParamSchema

-- | thyme UTCTime → delegate to time's UTCTime schema
instance ToSchema C.UTCTime where
  declareNamedSchema _ = do
    s <- declareNamedSchema (Proxy @Time.UTCTime)
    pure $ s { _namedSchemaName = Just "UTCTime" }

--------------------------------------------------------------------------------
-- ToSchema instances for API request/response types
--
-- Types with custom ToJSON instances use a simple freeform object schema.
-- This is pragmatic: the ToJSON instances hand-write field names that don't
-- match Haskell record fields, so Generic-based derivation would be wrong.
--------------------------------------------------------------------------------

freeformObject :: Text -> OA.NamedSchema
freeformObject name = NamedSchema (Just name)
  (mempty & OA.type_ ?~ OA.OpenApiObject)

-- | Aeson Value as freeform JSON — used by untyped response endpoints
instance ToSchema A.Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "JSONValue") mempty

-- | BIP70 binary data wrapper
instance ToSchema BIP70Data where
  declareNamedSchema _ = pure $ NamedSchema (Just "BIP70Data") OA.binarySchema

-- Auth
instance ToSchema AuthenticatedUser where
  declareNamedSchema _ = pure $ freeformObject "AuthenticatedUser"

instance ToSchema LoginRequest where
  declareNamedSchema _ = pure $ freeformObject "LoginRequest"

-- Users
instance ToSchema RegisterRequest where
  declareNamedSchema _ = pure $ freeformObject "RegisterRequest"

instance ToSchema RegisterResponse where
  declareNamedSchema _ = pure $ freeformObject "RegisterResponse"

instance ToSchema UsernameCheckResponse where
  declareNamedSchema _ = pure $ freeformObject "UsernameCheckResponse"

instance ToSchema ZAddrCheckResponse where
  declareNamedSchema _ = pure $ freeformObject "ZAddrCheckResponse"

instance ToSchema AccountSettingsResponse where
  declareNamedSchema _ = pure $ freeformObject "AccountSettingsResponse"

instance ToSchema SetPaymentAddressRequest where
  declareNamedSchema _ = pure $ freeformObject "SetPaymentAddressRequest"

-- Session
instance ToSchema LoginCheckResponse where
  declareNamedSchema _ = pure $ freeformObject "LoginCheckResponse"

-- Projects
instance ToSchema ProjectCreateRequest where
  declareNamedSchema _ = pure $ freeformObject "ProjectCreateRequest"

instance ToSchema ProjectCreateResponse where
  declareNamedSchema _ = pure $ freeformObject "ProjectCreateResponse"

instance ToSchema ProjectSummary where
  declareNamedSchema _ = pure $ freeformObject "ProjectSummary"

instance ToSchema ProjectResponse where
  declareNamedSchema _ = pure $ freeformObject "ProjectResponse"

instance ToSchema ProjectDetailResponse where
  declareNamedSchema _ = pure $ freeformObject "ProjectDetailResponse"

instance ToSchema ProjectInviteRequest where
  declareNamedSchema _ = pure $ freeformObject "ProjectInviteRequest"

instance ToSchema ProjectInviteResponse where
  declareNamedSchema _ = pure $ freeformObject "ProjectInviteResponse"

instance ToSchema CommsAddress where
  declareNamedSchema _ = pure $ freeformObject "CommsAddress"

-- WorkLog
instance ToSchema LogStartRequest where
  declareNamedSchema _ = pure $ freeformObject "LogStartRequest"

instance ToSchema LogEndRequest where
  declareNamedSchema _ = pure $ freeformObject "LogEndRequest"

instance ToSchema EventAmendmentRequest where
  declareNamedSchema _ = pure $ freeformObject "EventAmendmentRequest"

instance ToSchema ExtendedLogEntryResponse where
  declareNamedSchema _ = pure $ freeformObject "ExtendedLogEntryResponse"

instance ToSchema KeyedLogEntryResponse where
  declareNamedSchema _ = pure $ freeformObject "KeyedLogEntryResponse"

instance ToSchema WorkIndexResponse where
  declareNamedSchema _ = pure $ freeformObject "WorkIndexResponse"

instance ToSchema WorkIndexEntry where
  declareNamedSchema _ = pure $ freeformObject "WorkIndexEntry"

instance ToSchema IntervalResponse where
  declareNamedSchema _ = pure $ freeformObject "IntervalResponse"

instance ToSchema AmendEventResponse where
  declareNamedSchema _ = pure $ freeformObject "AmendEventResponse"

-- Auctions
instance ToSchema AuctionCreateRequest where
  declareNamedSchema _ = pure $ freeformObject "AuctionCreateRequest"

instance ToSchema AuctionCreateResponse where
  declareNamedSchema _ = pure $ freeformObject "AuctionCreateResponse"

instance ToSchema BidCreateRequest where
  declareNamedSchema _ = pure $ freeformObject "BidCreateRequest"

-- Billing
instance ToSchema BillableCreateRequest where
  declareNamedSchema _ = pure $ freeformObject "BillableCreateRequest"

instance ToSchema BillableCreateResponse where
  declareNamedSchema _ = pure $ freeformObject "BillableCreateResponse"

instance ToSchema BillableResponse where
  declareNamedSchema _ = pure $ freeformObject "BillableResponse"

instance ToSchema PaymentRequestCreateRequest where
  declareNamedSchema _ = pure $ freeformObject "PaymentRequestCreateRequest"

instance ToSchema PaymentRequestResponse where
  declareNamedSchema _ = pure $ freeformObject "PaymentRequestResponse"

instance ToSchema SubscribeRequest where
  declareNamedSchema _ = pure $ freeformObject "SubscribeRequest"

instance ToSchema SubscribeResponse where
  declareNamedSchema _ = pure $ freeformObject "SubscribeResponse"

-- Password Reset
instance ToSchema PasswordResetRequest where
  declareNamedSchema _ = pure $ freeformObject "PasswordResetRequest"

instance ToSchema PasswordResetResponse where
  declareNamedSchema _ = pure $ freeformObject "PasswordResetResponse"

instance ToSchema PasswordResetConfirm where
  declareNamedSchema _ = pure $ freeformObject "PasswordResetConfirm"

-- Config
instance ToSchema ClientConfig where
  declareNamedSchema _ = pure $ freeformObject "ClientConfig"
