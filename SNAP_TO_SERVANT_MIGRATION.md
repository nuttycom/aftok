# Snap to Servant Migration Plan

## Overview

This document outlines the plan for migrating the Aftok server from the Snap web
framework to Servant. The migration is motivated by dependency compatibility
issues - specifically `snaplet-postgresql-simple` and related Snap ecosystem
packages have tight version bounds that conflict with modern GHC and library
versions.

Servant is a type-safe web framework that represents APIs at the type level,
providing compile-time guarantees about routing, request/response types, and
API structure. It has a healthy ecosystem with active maintenance.

## Current Architecture Summary

### Snap Components in Use

| Component | Package | Purpose |
|-----------|---------|---------|
| Core Handler | `snap` | Request handling, routing |
| PostgreSQL | `snaplet-postgresql-simple` | Database connection pooling |
| Sessions | `snap` (SessionManager) | Cookie-based sessions |
| Auth | `snap` (AuthManager) | Username/password authentication |
| Static Files | `snap` | Serving static assets |

### Application Structure

```
App (Snaplet Container)
├── NetworkMode (Bitcoin/Zcash config)
├── SessionManager (Cookie sessions)
├── Postgres (Database pool)
└── AuthManager (Authentication state)
```

### Handler Modules (8 total)

1. `Aftok.Snaplet.Auth` - Login/logout, Basic Auth parsing
2. `Aftok.Snaplet.Users` - Registration, username validation, captcha
3. `Aftok.Snaplet.Projects` - Project CRUD, invitations, contributors
4. `Aftok.Snaplet.WorkLog` - Time logging, event amendments
5. `Aftok.Snaplet.Billing` - Billables, subscriptions, payment requests
6. `Aftok.Snaplet.Auctions` - Auction creation and bidding
7. `Aftok.Snaplet.Payments` - BIP-70 payment handling
8. `Aftok.Snaplet.Json` - JSON encoding utilities

### Route Count

Approximately 25-30 distinct endpoints across all modules.

---

## Migration Strategy

### Approach: Incremental Module-by-Module Migration

Rather than a big-bang rewrite, we'll migrate incrementally:

1. Set up Servant infrastructure alongside Snap
2. Migrate one handler module at a time
3. Use WAI to combine Snap and Servant applications during transition
4. Remove Snap components once all handlers are migrated

### Phase Overview

| Phase | Description | Estimated Complexity |
|-------|-------------|---------------------|
| 1 | Infrastructure Setup | Medium |
| 2 | Core Types & Monad Stack | Medium |
| 3 | Authentication Migration | High |
| 4 | Handler Migration (6 modules) | Medium each |
| 5 | Cleanup & Removal | Low |

---

## Phase 1: Infrastructure Setup

### 1.1 Add Servant Dependencies

Update `aftok.cabal`:

```cabal
build-depends:
  -- Servant core
  servant >= 0.20 && < 0.21,
  servant-server >= 0.20 && < 0.21,

  -- WAI ecosystem
  wai >= 3.2 && < 3.3,
  warp >= 3.3 && < 3.4,
  wai-extra >= 3.1 && < 3.2,
  wai-cors >= 0.2 && < 0.3,

  -- Authentication
  servant-auth >= 0.4 && < 0.5,
  servant-auth-server >= 0.4 && < 0.5,

  -- Session management (if needed)
  wai-session >= 0.3 && < 0.4,

  -- Existing (keep)
  postgresql-simple,
  resource-pool,
```

### 1.2 Create Parallel Directory Structure

```
server/
├── Main.hs                    # Keep for now, will be replaced
├── Aftok/
│   ├── Snaplet/               # Existing Snap handlers (keep during migration)
│   │   ├── Auth.hs
│   │   ├── Users.hs
│   │   └── ...
│   └── Servant/               # NEW: Servant handlers
│       ├── API.hs             # API type definition
│       ├── App.hs             # Application monad and context
│       ├── Auth.hs            # Authentication handlers
│       ├── Users.hs           # User handlers
│       └── ...
```

### 1.3 Create Servant Application Shell

Create `server/Aftok/Servant/App.hs`:

```haskell
module Aftok.Servant.App where

import Servant
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)

import Aftok.Currency.Bitcoin (NetworkMode)
import Aftok.Database (DBError)

-- Application environment (replaces Snap's App snaplet)
data AppEnv = AppEnv
  { envNetworkMode :: !NetworkMode
  , envDbPool :: !(Pool Connection)
  , envConfig :: !ServerConfig
  }

-- Application monad stack
type AppM = ReaderT AppEnv (ExceptT ServerError IO)

-- Natural transformation from AppM to Servant's Handler
appToHandler :: AppEnv -> AppM a -> Handler a
appToHandler env action = do
  result <- liftIO $ runExceptT $ runReaderT action env
  case result of
    Left err -> throwError err
    Right a -> pure a
```

---

## Phase 2: Core Types & Database Layer

### 2.1 Adapt Database Access

The existing `QDBM` monad can be preserved with minimal changes. Create an
adapter to run QDBM operations from Servant handlers:

```haskell
-- In server/Aftok/Servant/Database.hs

import Aftok.Database.PostgreSQL (QDBM, runQDBM)
import Data.Pool (withResource)

runDB :: QDBM a -> AppM a
runDB qdbm = do
  env <- ask
  let nmode = envNetworkMode env
      pool = envDbPool env
  result <- liftIO $ withResource pool $ \conn ->
    runExceptT $ runQDBM nmode conn qdbm
  case result of
    Left dbErr -> throwError $ dbErrorToServerError dbErr
    Right a -> pure a

dbErrorToServerError :: DBError -> ServerError
dbErrorToServerError = \case
  SubjectNotFound op -> err404 { errBody = "Resource not found" }
  OpForbidden _ reason -> err403 { errBody = encode reason }
  DBException e -> err500 { errBody = "Database error" }
```

### 2.2 Preserve Existing Business Logic

The library code in `lib/Aftok/` requires no changes:
- `Aftok.Types` - Core domain types
- `Aftok.Database` - Database operations (Program DBOp monad)
- `Aftok.TimeLog` - Time logging logic
- `Aftok.Billing` - Billing logic
- `Aftok.Payments` - Payment processing
- `Aftok.Project` - Project logic

---

## Phase 3: Authentication Migration

### 3.1 Authentication Strategy

Current Snap auth supports:
1. HTTP Basic Authentication (header-based)
2. JSON body login (XHR requests)
3. Cookie-based sessions

For Servant, we'll use `servant-auth-server` with:
1. Basic Auth combinator for API access
2. Cookie/Session auth for web UI
3. JWT tokens as an optional enhancement

### 3.2 Define Auth Types

```haskell
-- server/Aftok/Servant/Auth.hs

import Servant
import Servant.Auth.Server

-- Authenticated user info carried in requests
data AuthenticatedUser = AuthenticatedUser
  { auUserId :: UserId
  , auUsername :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

-- For JWT/cookie auth
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-- Auth configuration
type AftokAuth = Auth '[BasicAuth, Cookie, JWT] AuthenticatedUser

-- Protected endpoint combinator
type Protected api = AftokAuth :> api
```

### 3.3 Implement Auth Handlers

```haskell
-- Basic auth validation (replaces Snap's loginByUsername)
authCheck :: AppEnv -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck env (BasicAuthData username password) = do
  -- Look up user in database
  result <- runAppM env $ runDB $ findUserByUsername (decodeUtf8 username)
  case result of
    Left _ -> pure Indefinite
    Right Nothing -> pure NoSuchUser
    Right (Just user) ->
      if verifyPassword password (user ^. passwordHash)
        then pure $ Authenticated $ AuthenticatedUser (user ^. userId) (user ^. userName)
        else pure BadPassword

-- Login endpoint (for JSON body auth)
loginHandler :: LoginRequest -> AppM (Headers '[Header "Set-Cookie" SetCookie] AuthenticatedUser)
loginHandler (LoginRequest username password) = do
  -- Validate credentials and return session cookie
  ...
```

---

## Phase 4: Handler Migration

### Migration Order

Migrate in order of dependency (least dependent first):

1. **Users** - Registration, validation (no auth dependencies on other handlers)
2. **Projects** - Core resource, many handlers depend on it
3. **WorkLog** - Depends on Projects
4. **Auctions** - Depends on Projects
5. **Billing** - Depends on Projects
6. **Payments** - Depends on Billing

### 4.1 API Type Definition

Create `server/Aftok/Servant/API.hs`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.API where

import Servant
import Aftok.Servant.Auth (AftokAuth, AuthenticatedUser)

-- Top-level API
type AftokAPI =
       "api" :> "v1" :> VersionedAPI
  :<|> "static" :> Raw  -- Static file serving

type VersionedAPI =
       PublicAPI
  :<|> AftokAuth :> ProtectedAPI

-- Public endpoints (no auth required)
type PublicAPI =
       "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] UserId
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] AuthenticatedUser)
  :<|> "validate_username" :> QueryParam "username" Text :> Get '[JSON] UsernameAvailable
  :<|> "validate_zaddr" :> QueryParam "address" Text :> Get '[JSON] ZAddrValid

-- Protected endpoints (auth required)
type ProtectedAPI =
       ProjectsAPI
  :<|> WorkLogAPI
  :<|> BillingAPI
  :<|> AuctionsAPI
  :<|> PaymentsAPI

-- Projects sub-API
type ProjectsAPI = "projects" :>
  (    Get '[JSON] [(ProjectId, Project)]                           -- List projects
  :<|> ReqBody '[JSON] ProjectCreateRequest :> Post '[JSON] ProjectId  -- Create project
  :<|> Capture "projectId" ProjectId :>
       (    Get '[JSON] Project                                      -- Get project
       :<|> "detail" :> Get '[JSON] ProjectDetail                    -- Get project detail
       :<|> "payouts" :> Get '[JSON] WorkShares                      -- Get payouts
       :<|> "invite" :> ReqBody '[JSON] ProjectInviteRequest :> Post '[JSON] InviteResponse
       :<|> "workIndex" :> Get '[JSON] (WorkIndex KeyedLogEntry)
       )
  )

-- Work logging sub-API
type WorkLogAPI = "user" :> "projects" :> Capture "projectId" ProjectId :>
  (    "logStart" :> Post '[JSON] (ProjectId, UserId, KeyedLogEntry)
  :<|> "logEnd" :> Post '[JSON] (ProjectId, UserId, KeyedLogEntry)
  :<|> "events" :> QueryParam "after" UTCTime :> QueryParam "before" UTCTime :> Get '[JSON] [KeyedLogEntry]
  :<|> "workIndex" :> Get '[JSON] (WorkIndex KeyedLogEntry)
  )

-- Similar patterns for Billing, Auctions, Payments APIs...
```

### 4.2 Handler Implementation Pattern

For each Snap handler, create a corresponding Servant handler:

**Before (Snap):**
```haskell
projectCreateHandler :: S.Handler App App ProjectId
projectCreateHandler = do
  uid <- requireUserId
  requestBody <- readRequestBody 4096
  req <- either (snapError 400 . show) pure $ A.eitherDecode requestBody
  pid <- snapEval $ createProject $ Project (cpn req) uid (cpdepf req)
  pure pid
```

**After (Servant):**
```haskell
projectCreateHandler :: AuthenticatedUser -> ProjectCreateRequest -> AppM ProjectId
projectCreateHandler user req = do
  let uid = auUserId user
  runDB $ createProject $ Project (cpn req) uid (cpdepf req)
```

Key differences:
- Authentication comes via combinator, not explicit `requireUserId` call
- Request body parsed automatically by Servant
- Database access via `runDB` helper instead of `snapEval`
- Error handling via `AppM`'s `ExceptT` layer

### 4.3 Per-Module Migration Checklist

For each module:

- [ ] Create corresponding `Aftok.Servant.X` module
- [ ] Define sub-API type
- [ ] Implement handlers matching Snap handlers
- [ ] Add to main API composition
- [ ] Write integration tests
- [ ] Verify equivalent behavior
- [ ] Remove Snap handler module

---

## Phase 5: Cleanup & Removal

### 5.1 Remove Snap Dependencies

Once all handlers are migrated:

1. Remove from `aftok.cabal`:
   - `snap`
   - `snap-core`
   - `snap-server`
   - `snaplet-postgresql-simple`

2. Delete directories:
   - `server/Aftok/Snaplet/`

3. Update `server/Main.hs` to use only Servant

### 5.2 Final Server Setup

```haskell
-- server/Main.hs (final version)

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Data.Pool (createPool)

import Aftok.Servant.API
import Aftok.Servant.App

main :: IO ()
main = do
  cfg <- loadServerConfig
  pool <- createPool (connectPostgreSQL (cfg ^. dbConnString)) close 1 60 10
  let env = AppEnv
        { envNetworkMode = cfg ^. networkMode
        , envDbPool = pool
        , envConfig = cfg
        }
  let app = serveWithContext api (authContext env) (server env)
  run (cfg ^. port) $ middleware app

middleware :: Application -> Application
middleware = cors corsPolicy . logStdout

server :: AppEnv -> Server AftokAPI
server env = hoistServerWithContext api (Proxy :: Proxy AuthContext)
  (appToHandler env)
  handlers

handlers :: ServerT AftokAPI AppM
handlers = publicHandlers :<|> protectedHandlers
```

---

## Testing Strategy

### Unit Tests

- Keep existing QuickCheck property tests in `test/`
- Add Servant-specific handler unit tests

### Integration Tests

Use `servant-client` to generate API clients for testing:

```haskell
-- test/Integration/API.hs

import Servant.Client

-- Auto-generated client functions
register :<|> login :<|> validateUsername :<|> ... = client (Proxy :: Proxy AftokAPI)

spec :: Spec
spec = around withTestApp $ do
  describe "Registration" $ do
    it "creates a new user" $ \port -> do
      let req = RegisterRequest "testuser" "password123" Nothing []
      result <- runClientM (register req) (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
      result `shouldSatisfy` isRight
```

### Smoke Tests

During migration, maintain the ability to run both servers to compare behavior.

---

## Risk Mitigation

### Risk: Session Incompatibility

**Concern:** Existing user sessions may not transfer between Snap and Servant.

**Mitigation:**
- Plan a maintenance window for the cutover
- Alternatively, implement session format compatibility

### Risk: Authentication Edge Cases

**Concern:** Subtle differences in auth handling (timing, error messages).

**Mitigation:**
- Comprehensive auth test suite
- Side-by-side testing during migration

### Risk: Performance Regression

**Concern:** Different performance characteristics between frameworks.

**Mitigation:**
- Benchmark critical endpoints before and after
- Load testing before production cutover

### Risk: Database Connection Handling

**Concern:** Different pooling behavior between snaplet and direct pool.

**Mitigation:**
- Use same pool configuration
- Monitor connection usage during testing

---

## Timeline Considerations

This is a significant refactoring effort. Rough estimates:

| Phase | Effort |
|-------|--------|
| Phase 1: Infrastructure | 1-2 days |
| Phase 2: Core Types | 1 day |
| Phase 3: Authentication | 2-3 days |
| Phase 4: Handler Migration | 1 day per module (6 modules) |
| Phase 5: Cleanup | 1 day |

**Total: Approximately 2-3 weeks of focused effort**

---

## Open Questions

1. **JWT vs Sessions**: Should we adopt JWT tokens for API auth, or maintain
   cookie-based sessions only?

2. **API Versioning**: Should we use this as an opportunity to version the API
   (`/api/v1/...`) for future flexibility?

3. **OpenAPI Documentation**: Servant can auto-generate OpenAPI specs. Is this
   valuable for the project?

4. **Daemon Migration**: The `aftok-daemon` also uses some Snap-adjacent code.
   Does it need updates?

---

## References

- [Servant Documentation](https://docs.servant.dev/)
- [servant-auth](https://hackage.haskell.org/package/servant-auth)
- [servant-auth-server](https://hackage.haskell.org/package/servant-auth-server)
- [WAI (Web Application Interface)](https://hackage.haskell.org/package/wai)
- [Warp Server](https://hackage.haskell.org/package/warp)
