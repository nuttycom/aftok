{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Session
  ( -- * API Types
    SessionAPI,
    ProtectedSessionAPI,

    -- * Handlers
    sessionServer,
    protectedSessionServer,
  )
where

import Aftok.Database (findUserByName)
import Aftok.Servant.App (AppEnv (..), AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..), LoginRequest (..))
import Aftok.Types (UserName (..), username, _UserName)
import Control.Lens ((^.))
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import Servant
import Servant.Auth.Server
  ( AuthResult (..),
    SetCookie,
    acceptLogin,
    clearSession,
  )

-- | Session API (public endpoints)
type SessionAPI =
  -- POST /login - authenticate and get session cookie
  "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    -- GET /logout - clear session
    :<|> "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

-- | Protected Session API (requires auth)
type ProtectedSessionAPI =
  -- GET /login/check - verify current session
  "login" :> "check" :> Get '[JSON] LoginCheckResponse

-- | Login check response
data LoginCheckResponse = LoginCheckResponse
  { loggedIn :: Bool,
    loginUser :: Maybe AuthenticatedUser
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginCheckResponse where
  toJSON (LoginCheckResponse li user) =
    A.object
      [ "loggedIn" .= li,
        "user" .= user
      ]

-- | Session server implementation (public endpoints)
sessionServer :: ServerT SessionAPI AppM
sessionServer =
  loginHandler
    :<|> logoutHandler

-- | Protected session server (check login)
protectedSessionServer :: AuthResult AuthenticatedUser -> ServerT ProtectedSessionAPI AppM
protectedSessionServer (Authenticated user) = loginCheckHandler (Just user)
protectedSessionServer _ = loginCheckHandler Nothing

-- | Handle login request
loginHandler ::
  LoginRequest ->
  AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler (LoginRequest usernameText _password) = do
  env <- ask
  let cookieSettings = _envCookieSettings env
      jwtSettings = _envJWTSettings env

  -- Find user by username
  userResult <- runDB $ runMaybeT $ findUserByName (UserName usernameText)

  case userResult of
    Nothing ->
      throwError err401 {errBody = "User not found"}
    Just (uid, userRec) -> do
      -- TODO: Add actual password verification here
      let authedUser = AuthenticatedUser uid (userRec ^. username . _UserName)

      -- Create session cookie/JWT
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings authedUser
      case mApplyCookies of
        Nothing ->
          throwError err500 {errBody = "Failed to create session"}
        Just applyCookies ->
          pure $ applyCookies NoContent

-- | Handle logout request
logoutHandler ::
  AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logoutHandler = do
  env <- ask
  let cookieSettings = _envCookieSettings env
  pure $ clearSession cookieSettings NoContent

-- | Handle login check
loginCheckHandler :: Maybe AuthenticatedUser -> AppM LoginCheckResponse
loginCheckHandler mUser =
  pure $ LoginCheckResponse (isJust mUser) mUser
