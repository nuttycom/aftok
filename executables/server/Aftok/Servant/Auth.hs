{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Aftok.Servant.Auth
  ( -- * Types (re-exported from aftok-api)
    AuthenticatedUser (..),
    LoginRequest (..),
    AftokAuth,

    -- * Auth checking
    requireAuth,
  )
where

import Aftok.API.Auth (AftokAuth, AuthenticatedUser (..), LoginRequest (..))
import Aftok.Database (findUserByNameWithPassword)
import Aftok.Database.PostgreSQL (runQDBM)
import Aftok.Password (verifyPassword)
import Aftok.Servant.App (AppEnv (..))
import Aftok.Types (UserName (..), username, _UserName)
import Control.Error.Util (hush)
import Control.Lens ((^.))
import Data.Pool (withResource)
import Servant (err401, err403)
import Servant.API.BasicAuth (BasicAuthData (..))
import Servant.Auth.Server
  ( AuthResult (..),
    BasicAuthCfg,
    FromBasicAuthData (..),
  )
import Servant.Server (ServerError (..))

-- | Type alias for BasicAuth config (needed by servant-auth-server)
type instance BasicAuthCfg = AppEnv

-- | Check basic auth credentials
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData (BasicAuthData usernameBytes passwordBytes) env = do
    let nmode = _envNetworkMode env
        pool = _envDbPool env
    result <- withResource pool $ \conn ->
      runExceptT $
        runQDBM nmode conn $
          runMaybeT $
            findUserByNameWithPassword (UserName $ decodeUtf8 usernameBytes)
    case hush result of
      Nothing -> pure Indefinite
      Just Nothing -> pure NoSuchUser
      Just (Just (uid, user, mPwdHash)) ->
        case mPwdHash of
          Nothing ->
            -- User has no password set
            pure BadPassword
          Just pwdHash ->
            if verifyPassword passwordBytes pwdHash
              then
                let uname = user ^. username . _UserName
                 in pure $ Authenticated $ AuthenticatedUser uid uname
              else pure BadPassword

-- | Require authentication, throwing an error if not authenticated
requireAuth :: AuthResult AuthenticatedUser -> Either ServerError AuthenticatedUser
requireAuth (Authenticated user) = Right user
requireAuth NoSuchUser = Left err401 {errBody = "User not found"}
requireAuth BadPassword = Left err403 {errBody = "Invalid password"}
requireAuth Indefinite = Left err401 {errBody = "Authentication required"}
