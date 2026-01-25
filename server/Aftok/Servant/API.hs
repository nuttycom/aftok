{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.API
  ( -- * Top-level API
    AftokAPI,

    -- * Component APIs (re-exported from handler modules)
    module Aftok.Servant.Users,
    module Aftok.Servant.Projects,
    module Aftok.Servant.WorkLog,
    module Aftok.Servant.Auctions,
    module Aftok.Servant.Billing,
    module Aftok.Servant.Payments,

    -- * Proxy
    aftokAPI,
  )
where

import Aftok.Servant.Auth (AftokAuth)
import Aftok.Servant.Auctions
  ( AuctionsAPI,
    ProjectAuctionsAPI,
    ProtectedAuctionsAPI,
  )
import Aftok.Servant.Billing
  ( BillingAPI,
    ProjectBillablesAPI,
    ProtectedBillingAPI,
  )
import Aftok.Servant.Payments
  ( PaymentsAPI,
    ProtectedPaymentsAPI,
  )
import Aftok.Servant.Projects
  ( ProjectsAPI,
    ProtectedProjectsAPI,
  )
import Aftok.Servant.Users
  ( ProtectedUsersAPI,
    UsersAPI,
  )
import Aftok.Servant.WorkLog (WorkLogAPI)
import Servant

-- | Top-level API combining all routes
type AftokAPI =
  "api" :> VersionedAPI
    :<|> "static" :> Raw

-- | Versioned API (could add v2, v3, etc. later)
type VersionedAPI =
  -- Public endpoints (no auth required)
  UsersAPI
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

-- | Proxy for the API (used for serving)
aftokAPI :: Proxy AftokAPI
aftokAPI = Proxy
