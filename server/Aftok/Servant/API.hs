{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.API
  ( -- * Top-level API
    AftokAPI,
    VersionedAPI,

    -- * Public API
    PublicAPI,

    -- * Protected APIs
    ProtectedAPI,
    ProjectsAPI,
    UserProjectsAPI,
    AuctionsAPI,
    BillingAPI,
    PaymentsAPI,

    -- * Proxy
    aftokAPI,
  )
where

import Aftok.Auction (AuctionId)
import Aftok.Billing (BillableId, SubscriptionId)
import Aftok.Payments.Types (PaymentRequestId)
import Aftok.Servant.Auth (AftokAuth, LoginRequest)
import Aftok.TimeLog (EventId)
import Aftok.Types (ProjectId, UserId)
import Data.Aeson (Value)
import Data.Thyme.Clock (UTCTime)
import Servant

-- | Top-level API combining all routes
type AftokAPI =
  "api" :> VersionedAPI
    :<|> "static" :> Raw

-- | Versioned API (could add v2, v3, etc. later)
type VersionedAPI =
  PublicAPI
    :<|> AftokAuth :> ProtectedAPI

-- | Public endpoints (no auth required)
type PublicAPI =
  -- POST /login - XHR login
  "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Value
    -- GET /login/check - Check if logged in
    :<|> "login" :> "check" :> Get '[JSON] Value
    -- POST /register - Register new user
    :<|> "register" :> ReqBody '[JSON] Value :> Post '[JSON] UserId
    -- GET /validate_username?username=...
    :<|> "validate_username" :> QueryParam "username" Text :> Get '[JSON] Value
    -- GET /validate_zaddr?address=...
    :<|> "validate_zaddr" :> QueryParam "address" Text :> Get '[JSON] Value
    -- POST /accept_invitation
    :<|> "accept_invitation" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- | Protected endpoints (auth required)
type ProtectedAPI =
  -- GET /logout
  "logout" :> Get '[JSON] NoContent
    -- Projects
    :<|> ProjectsAPI
    -- User-specific project operations
    :<|> UserProjectsAPI
    -- Auctions
    :<|> AuctionsAPI
    -- Billing
    :<|> BillingAPI
    -- Payments
    :<|> PaymentsAPI
    -- Events
    :<|> EventsAPI

-- | Projects API
type ProjectsAPI =
  "projects"
    :> ( -- GET /projects - List projects
         Get '[JSON] Value
           -- POST /projects - Create project
           :<|> ReqBody '[JSON] Value :> Post '[JSON] ProjectId
           -- Project-specific routes
           :<|> Capture "projectId" ProjectId :> ProjectAPI
       )

-- | Single project operations
type ProjectAPI =
  -- GET /projects/:projectId
  Get '[JSON] Value
    -- GET /projects/:projectId/detail
    :<|> "detail" :> Get '[JSON] Value
    -- GET /projects/:projectId/payouts
    :<|> "payouts" :> Get '[JSON] Value
    -- GET /projects/:projectId/workIndex
    :<|> "workIndex" :> Get '[JSON] Value
    -- POST /projects/:projectId/invite
    :<|> "invite" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    -- Auctions within a project
    :<|> "auctions"
      :> ( Get '[JSON] Value
             :<|> ReqBody '[JSON] Value :> Post '[JSON] AuctionId
         )
    -- Billables within a project
    :<|> "billables"
      :> ( Get '[JSON] Value
             :<|> ReqBody '[JSON] Value :> Post '[JSON] BillableId
             :<|> Capture "billableId" BillableId
               :> "paymentRequests"
               :> ReqBody '[JSON] Value
               :> Post '[JSON] Value
         )

-- | User-specific project operations
type UserProjectsAPI =
  "user"
    :> "projects"
    :> Capture "projectId" ProjectId
    :> ( -- POST /user/projects/:projectId/logStart
         "logStart" :> Post '[JSON] Value
           -- POST /user/projects/:projectId/logEnd
           :<|> "logEnd" :> Post '[JSON] Value
           -- GET /user/projects/:projectId/events
           :<|> "events"
             :> QueryParam "after" UTCTime
             :> QueryParam "before" UTCTime
             :> Get '[JSON] Value
           -- GET /user/projects/:projectId/workIndex
           :<|> "workIndex" :> Get '[JSON] Value
       )

-- | Auctions API
type AuctionsAPI =
  "auctions"
    :> Capture "auctionId" AuctionId
    :> ( -- GET /auctions/:auctionId
         Get '[JSON] Value
           -- POST /auctions/:auctionId/bid
           :<|> "bid" :> ReqBody '[JSON] Value :> Post '[JSON] Value
       )

-- | Billing API
type BillingAPI =
  "subscribe"
    :> Capture "billableId" BillableId
    :> ReqBody '[JSON] Value
    :> Post '[JSON] SubscriptionId

-- | Payments API
type PaymentsAPI =
  "pay"
    :> "btc"
    :> Capture "paymentRequestKey" Text
    :> ( -- GET /pay/btc/:paymentRequestKey - Get BIP70 payment request
         Get '[OctetStream] ByteString
           -- POST /pay/btc/:paymentRequestKey - Submit BIP70 payment
           :<|> ReqBody '[OctetStream] ByteString :> Post '[JSON] PaymentRequestId
       )

-- | Events API
type EventsAPI =
  "events"
    :> Capture "eventId" EventId
    :> "amend"
    :> ReqBody '[JSON] Value
    :> Put '[JSON] Value

-- | Proxy for the API (used for serving)
aftokAPI :: Proxy AftokAPI
aftokAPI = Proxy
