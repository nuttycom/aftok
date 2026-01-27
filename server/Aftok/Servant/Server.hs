{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Server
  ( -- * Server
    aftokServer,
    aftokApp,

    -- * Configuration
    mkAppEnv,
  )
where

import qualified Aftok.Config as AC
import Aftok.Currency.Bitcoin (NetworkMode)
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Payments (PaymentsConfig)
import Aftok.ServerConfig (ServerConfig, secureCookies)
import Control.Lens ((^.))
import Aftok.Servant.API (AftokAPI, aftokAPI)
import Aftok.Servant.App (AppEnv (..), AppM, appToHandler)
import Aftok.Servant.Instances ()
import Aftok.Servant.Auth (AftokAuth, AuthenticatedUser)
import Aftok.Servant.Auctions
  ( ProtectedAuctionsAPI,
    protectedAuctionsServer,
  )
import Aftok.Servant.Billing
  ( ProtectedBillingAPI,
    protectedBillingServer,
  )
import Aftok.Servant.Payments
  ( ProtectedPaymentsAPI,
    protectedPaymentsServer,
  )
import Aftok.Servant.Projects
  ( ProtectedProjectsAPI,
    protectedProjectsServer,
  )
import Aftok.Servant.Users
  ( CaptchaConfig,
    ProtectedUsersAPI,
    RegisterOps,
    UsersAPI,
    acceptInvitationHandler,
    usersServer,
  )
import Aftok.Servant.Session
  ( ProtectedSessionAPI,
    SessionAPI,
    protectedSessionServer,
    sessionServer,
  )
import Aftok.Servant.PasswordReset
  ( PasswordResetAPI,
    PasswordResetOps,
    passwordResetServer,
  )
import Aftok.Servant.WorkLog (WorkLogAPI, workLogServer)
import Crypto.JOSE.JWK (JWK)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server
  ( AuthResult (..),
    CookieSettings (..),
    JWTSettings,
    SameSite (..),
    XsrfCookieSettings (..),
    defaultCookieSettings,
    defaultJWTSettings,
    defaultXsrfCookieSettings,
  )

-- | Create application environment
mkAppEnv ::
  NetworkMode ->
  Pool Connection ->
  ServerConfig ->
  JWK ->
  AppEnv
mkAppEnv nmode pool cfg jwk =
  AppEnv
    { _envNetworkMode = nmode,
      _envDbPool = pool,
      _envConfig = cfg,
      _envCookieSettings = defaultCookieSettings
        { cookieIsSecure = if cfg ^. secureCookies then Secure else NotSecure
        , cookieSameSite = SameSiteStrict
        , cookiePath = Just "/"
        , cookieMaxAge = Just 86400  -- 24 hours
        , cookieXsrfSetting = Just defaultXsrfCookieSettings
            { xsrfExcludeGet = True  -- Don't require XSRF token for GET requests
            }
        },
      _envJWTSettings = defaultJWTSettings jwk
    }

-- | Full Aftok server
aftokServer ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  RegisterOps IO ->
  CaptchaConfig ->
  PasswordResetOps IO ->
  FilePath ->
  ServerT AftokAPI AppM
aftokServer btcCfg payCfg regOps captchaCfg pwResetOps staticDir =
  apiServer btcCfg payCfg regOps captchaCfg pwResetOps
    :<|> serveDirectoryWebApp staticDir

-- | Protected API combines all protected endpoints
type ProtectedAPI =
  ProtectedProjectsAPI
    :<|> WorkLogAPI
    :<|> ProtectedAuctionsAPI
    :<|> ProtectedBillingAPI
    :<|> ProtectedPaymentsAPI
    :<|> ProtectedUsersAPI
    :<|> ProtectedSessionAPI

-- | API server (without static files)
apiServer ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  RegisterOps IO ->
  CaptchaConfig ->
  PasswordResetOps IO ->
  ServerT (UsersAPI :<|> SessionAPI :<|> PasswordResetAPI :<|> AftokAuth :> ProtectedAPI) AppM
apiServer btcCfg payCfg regOps captchaCfg pwResetOps =
  usersServer regOps captchaCfg
    :<|> sessionServer
    :<|> passwordResetServer pwResetOps
    :<|> protectedServer btcCfg payCfg

-- | Protected server (requires authentication)
protectedServer ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ServerT ProtectedAPI AppM
protectedServer btcCfg payCfg authResult =
  protectedProjectsServer payCfg authResult
    :<|> workLogServer authResult
    :<|> protectedAuctionsServer authResult
    :<|> protectedBillingServer authResult
    :<|> protectedPaymentsServer btcCfg payCfg authResult
    :<|> protectedUsersServer authResult
    :<|> protectedSessionServer authResult

-- | Protected users server (just accept invitation handler)
protectedUsersServer ::
  AuthResult AuthenticatedUser ->
  ServerT ProtectedUsersAPI AppM
protectedUsersServer (Authenticated user) = acceptInvitationHandler user
protectedUsersServer _ = \_ -> throwError err401 {errBody = "Authentication required"}

-- | Create WAI Application from server
aftokApp ::
  AppEnv ->
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  RegisterOps IO ->
  CaptchaConfig ->
  PasswordResetOps IO ->
  FilePath ->
  Application
aftokApp env btcCfg payCfg regOps captchaCfg pwResetOps staticDir =
  serveWithContext
    aftokAPI
    (authContext env)
    (hoistServerWithContext aftokAPI contextProxy (appToHandler env) server)
  where
    server = aftokServer btcCfg payCfg regOps captchaCfg pwResetOps staticDir

-- | Auth context for servant-auth-server
authContext :: AppEnv -> Context '[CookieSettings, JWTSettings, AppEnv]
authContext env =
  _envCookieSettings env
    :. _envJWTSettings env
    :. env
    :. EmptyContext

-- | Proxy for context types
contextProxy :: Proxy '[CookieSettings, JWTSettings, AppEnv]
contextProxy = Proxy
