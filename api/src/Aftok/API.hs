{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level Aftok API types.
-- This module re-exports all API types from component modules.
module Aftok.API
  ( -- * Top-level API
    AftokAPI,
    VersionedAPI,
    ProtectedAPI,

    -- * Component APIs
    module Aftok.API.Users,
    module Aftok.API.Session,
    module Aftok.API.Projects,
    module Aftok.API.WorkLog,
    module Aftok.API.Auctions,
    module Aftok.API.Billing,
    module Aftok.API.Payments,
    module Aftok.API.PasswordReset,
    module Aftok.API.Config,

    -- * Auth Types
    module Aftok.API.Auth,

    -- * Proxy
    aftokAPI,
  )
where

import Aftok.API.Auctions
import Aftok.API.Auth
import Aftok.API.Billing
import Aftok.API.Config
import Aftok.API.PasswordReset
import Aftok.API.Payments
import Aftok.API.Projects
import Aftok.API.Session
import Aftok.API.Types ()
import Aftok.API.Users
import Aftok.API.WorkLog
import Servant.API

-- | Top-level API combining all routes
type AftokAPI =
  "api" :> VersionedAPI
    :<|> "static" :> Raw

-- | Versioned API (could add v2, v3, etc. later)
type VersionedAPI =
  -- Public endpoints (no auth required)
  UsersAPI
    -- Session endpoints (login/logout - public)
    :<|> SessionAPI
    -- Password reset endpoints (public)
    :<|> PasswordResetAPI
    -- Client configuration endpoint (public)
    :<|> ConfigAPI
    -- Protected endpoints (auth required)
    :<|> AftokAuth :> ProtectedAPI

-- | Protected endpoints (auth required)
type ProtectedAPI =
  -- Projects (list, create, single project operations)
  ProtectedProjectsAPI
    -- WorkLog (user project operations + event amendments)
    :<|> WorkLogAPI
    -- Auctions (single auction operations)
    :<|> ProtectedAuctionsAPI
    -- Billing (subscriptions)
    :<|> ProtectedBillingAPI
    -- Payments (BIP70)
    :<|> ProtectedPaymentsAPI
    -- User operations (accept invitation)
    :<|> ProtectedUsersAPI
    -- Session operations (login check)
    :<|> ProtectedSessionAPI

-- | Proxy for the API (used for serving and client generation)
aftokAPI :: Proxy AftokAPI
aftokAPI = Proxy
