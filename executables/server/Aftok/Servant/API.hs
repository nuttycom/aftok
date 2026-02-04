{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.API
  ( -- * Top-level API (re-exported from aftok-api)
    AftokAPI,
    VersionedAPI,
    ProtectedAPI,
    aftokAPI,

    -- * Component APIs (re-exported from handler modules)
    module Aftok.Servant.Users,
    module Aftok.Servant.Projects,
    module Aftok.Servant.WorkLog,
    module Aftok.Servant.Auctions,
    module Aftok.Servant.Billing,
    module Aftok.Servant.Payments,
    module Aftok.Servant.Session,
    module Aftok.Servant.PasswordReset,
    module Aftok.Servant.Config,
  )
where

import Aftok.API
  ( AftokAPI,
    ProtectedAPI,
    VersionedAPI,
    aftokAPI,
  )
import Aftok.Servant.Auctions
import Aftok.Servant.Billing
import Aftok.Servant.Config
import Aftok.Servant.PasswordReset
import Aftok.Servant.Payments
import Aftok.Servant.Projects
import Aftok.Servant.Session
import Aftok.Servant.Users
import Aftok.Servant.WorkLog
