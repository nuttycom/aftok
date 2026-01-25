{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Auth
  ( -- * Types
    AuthenticatedUser (..),
    LoginRequest (..),

    -- * Auth combinators
    AftokAuth,

    -- * Auth checking
    requireAuth,
  )
where

import Aftok.Database (findUserByName)
import Aftok.Database.PostgreSQL (runQDBM)
import Aftok.Servant.App (AppEnv (..))
import Aftok.Types (UserId (..), UserName (..), username, _UserName)
import Control.Error.Util (hush)
import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import Data.Pool (withResource)
import Servant (err401, err403)
import Servant.Auth.Server
  ( Auth,
    AuthResult (..),
    BasicAuth,
    BasicAuthCfg,
    Cookie,
    FromBasicAuthData (..),
    FromJWT,
    JWT,
    ToJWT,
  )
import Servant.API.BasicAuth (BasicAuthData (..))
import Servant.Server (ServerError (..))

-- | Authenticated user info carried in requests
data AuthenticatedUser = AuthenticatedUser
  { auUserId :: !UserId,
    auUsername :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON AuthenticatedUser where
  toJSON (AuthenticatedUser (UserId uid) uname) =
    A.object
      [ "userId" .= UUID.toText uid,
        "username" .= uname
      ]

instance FromJSON AuthenticatedUser where
  parseJSON = A.withObject "AuthenticatedUser" $ \o -> do
    uidText <- o .: "userId"
    uid <- case UUID.fromText uidText of
      Nothing -> fail "Invalid UUID for userId"
      Just u -> pure $ UserId u
    uname <- o .: "username"
    pure $ AuthenticatedUser uid uname

-- For JWT/cookie auth
instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

-- | Login request body
data LoginRequest = LoginRequest
  { loginUser :: !Text,
    loginPass :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginRequest where
  parseJSON (A.Object o) =
    LoginRequest <$> o .: "username" <*> o .: "password"
  parseJSON val = fail $ "Value " <> show val <> " is not a JSON object."

-- | Auth configuration type for servant-auth
type AftokAuth = Auth '[BasicAuth, Cookie, JWT] AuthenticatedUser

-- | Type alias for BasicAuth config (needed by servant-auth-server)
type instance BasicAuthCfg = AppEnv

-- | Check basic auth credentials
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData (BasicAuthData usernameBytes _password) env = do
    let nmode = _envNetworkMode env
        pool = _envDbPool env
    result <- withResource pool $ \conn ->
      runExceptT $ runQDBM nmode conn $
        runMaybeT $ findUserByName (UserName $ decodeUtf8 usernameBytes)
    case hush result of
      Nothing -> pure Indefinite
      Just Nothing -> pure NoSuchUser
      Just (Just (uid, user)) ->
        -- TODO: Add actual password verification here
        let uname = user ^. username . _UserName
         in pure $ Authenticated $ AuthenticatedUser uid uname

-- | Require authentication, throwing an error if not authenticated
requireAuth :: AuthResult AuthenticatedUser -> Either ServerError AuthenticatedUser
requireAuth (Authenticated user) = Right user
requireAuth NoSuchUser = Left err401 {errBody = "User not found"}
requireAuth BadPassword = Left err403 {errBody = "Invalid password"}
requireAuth Indefinite = Left err401 {errBody = "Authentication required"}
