{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | OpenAPI 3 schema instances for all Aftok API types.
module Aftok.API.OpenApi () where

import Aftok.API.Auctions
  ( AuctionCreateRequest,
    AuctionCreateResponse,
    BidCreateRequest,
  )
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
import Aftok.API.Codec ()
import Aftok.API.Config (ClientConfig)
import Aftok.API.GitHub
  ( GitHubOAuthInitResponse,
    GitHubUsernameResponse,
    GitHubWebhookPayload (..),
    LinkRepoRequest,
    LinkRepoResponse,
    RepoLinkInfo,
  )
import Aftok.API.PasswordReset
  ( PasswordResetConfirm,
    PasswordResetRequest,
    PasswordResetResponse,
  )
import Aftok.API.Payments (BIP70Data (..))
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
import Aftok.Auction (AuctionId (..))
import Aftok.Billing (BillableId (..), SubscriptionId (..))
import Aftok.Payments.Types (PaymentId (..))
import Aftok.Types (GitHubRepoLinkId (..), ProjectId (..), UserId (..))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Control.Lens ((.~), (?~))
import qualified Data.Aeson as A
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
      & OA.security
      .~ [OA.SecurityRequirement mempty]
        & OA.components
        . OA.securitySchemes
      .~ OA.SecurityDefinitions mempty

--------------------------------------------------------------------------------
-- ToParamSchema instances for ID types (used in URL captures)
--------------------------------------------------------------------------------

uuidParamSchema :: OA.Schema
uuidParamSchema =
  mempty
    & OA.type_
    ?~ OA.OpenApiString
      & OA.format
    ?~ "uuid"

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
  toParamSchema _ =
    mempty
      & OA.type_
      ?~ OA.OpenApiString
        & OA.format
      ?~ "date-time"

--------------------------------------------------------------------------------
-- ToSchema instances for core types (codec-derived)
--------------------------------------------------------------------------------

instance ToSchema ProjectId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema UserId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema BillableId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema AuctionId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema SubscriptionId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema PaymentId where
  declareNamedSchema = declareNamedSchemaViaCodec

-- | thyme UTCTime → delegate to time's UTCTime schema
instance ToSchema C.UTCTime where
  declareNamedSchema _ = do
    s <- declareNamedSchema (Proxy @Time.UTCTime)
    pure $ s {_namedSchemaName = Just "UTCTime"}

--------------------------------------------------------------------------------
-- ToSchema instances for special types
--------------------------------------------------------------------------------

-- | Aeson Value as freeform JSON — used by untyped response endpoints
instance ToSchema A.Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "JSONValue") mempty

-- | BIP70 binary data wrapper
instance ToSchema BIP70Data where
  declareNamedSchema _ = pure $ NamedSchema (Just "BIP70Data") OA.binarySchema

instance ToSchema GitHubWebhookPayload where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "GitHubWebhookPayload") OA.binarySchema

--------------------------------------------------------------------------------
-- ToSchema instances for API request/response types (codec-derived)
--------------------------------------------------------------------------------

-- Auth
instance ToSchema AuthenticatedUser where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema LoginRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Users
instance ToSchema RegisterRequest where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "RegisterRequest")
        (mempty & OA.type_ ?~ OA.OpenApiObject)

instance ToSchema RegisterResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema UsernameCheckResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ZAddrCheckResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema AccountSettingsResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema SetPaymentAddressRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Session
instance ToSchema LoginCheckResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Projects
instance ToSchema ProjectCreateRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectCreateResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectSummary where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectDetailResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectInviteRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema ProjectInviteResponse where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "ProjectInviteResponse")
        (mempty & OA.type_ ?~ OA.OpenApiObject)

instance ToSchema CommsAddress where
  declareNamedSchema = declareNamedSchemaViaCodec

-- WorkLog
instance ToSchema LogStartRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema LogEndRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema EventAmendmentRequest where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "EventAmendmentRequest")
        (mempty & OA.type_ ?~ OA.OpenApiObject)

instance ToSchema ExtendedLogEntryResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema KeyedLogEntryResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema WorkIndexResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema WorkIndexEntry where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema IntervalResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema AmendEventResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Auctions
instance ToSchema AuctionCreateRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema AuctionCreateResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema BidCreateRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Billing
instance ToSchema BillableCreateRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema BillableCreateResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema BillableResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema PaymentRequestCreateRequest where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "PaymentRequestCreateRequest")
        (mempty & OA.type_ ?~ OA.OpenApiObject)

instance ToSchema PaymentRequestResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema SubscribeRequest where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "SubscribeRequest")
        (mempty & OA.type_ ?~ OA.OpenApiObject)

instance ToSchema SubscribeResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Password Reset
instance ToSchema PasswordResetRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema PasswordResetResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema PasswordResetConfirm where
  declareNamedSchema = declareNamedSchemaViaCodec

-- Config
instance ToSchema ClientConfig where
  declareNamedSchema = declareNamedSchemaViaCodec

-- GitHub
instance ToSchema GitHubRepoLinkId where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema LinkRepoRequest where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema LinkRepoResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema RepoLinkInfo where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema GitHubOAuthInitResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

instance ToSchema GitHubUsernameResponse where
  declareNamedSchema = declareNamedSchemaViaCodec
