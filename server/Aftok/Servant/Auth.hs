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
    checkBasicAuth,
    requireAuth,
  )
where

import Aftok.Database (findUser, findUserByName)
import Aftok.Servant.App (AppEnv (..), runDB)
import Aftok.Types (User, UserId, UserName (..), userEmail, userId)
import Control.Error.Util (hush)
import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as A
import Data.Pool (withResource)
import Servant (err401, err403, errBody)
import Servant.Auth.Server
  ( AuthResult (..),
    BasicAuthCfg,
    CookieSettings,
    FromBasicAuthData (..),
    FromJWT,
    JWTSettings,
    ToJWT,
  )
import Servant.Auth.Server.Internal.Types (Auth)
import Servant.Server (ServerError (..))

import Aftok.Database.PostgreSQL (runQDBM)

-- | Authenticated user info carried in requests
data AuthenticatedUser = AuthenticatedUser
  { auUserId :: !UserId,
    auUsername :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

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
type AftokAuth = Auth '[Servant.Auth.Server.Internal.Types.BasicAuth, Servant.Auth.Server.Internal.Types.Cookie, Servant.Auth.Server.Internal.Types.JWT] AuthenticatedUser

-- | Type alias for BasicAuth config (needed by servant-auth-server)
type instance BasicAuthCfg = AppEnv

-- | Check basic auth credentials
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData (username, password) env = do
    let nmode = _envNetworkMode env
        pool = _envDbPool env
    result <- withResource pool $ \conn ->
      runExceptT $ runQDBM nmode conn $ do
        userMay <- findUserByName (UserName $ decodeUtf8 username)
        case userMay of
          Nothing -> pure Nothing
          Just (uid, user) -> do
            -- TODO: Implement proper password verification
            -- For now, we need to look up the password hash and verify
            pure $ Just (uid, user)
    case hush result >>= join of
      Nothing -> pure Indefinite
      Just (uid, user) ->
        -- TODO: Add actual password verification here
        pure $ Authenticated $ AuthenticatedUser uid (user ^. userEmail)

-- | Require authentication, throwing an error if not authenticated
requireAuth :: AuthResult AuthenticatedUser -> Either ServerError AuthenticatedUser
requireAuth (Authenticated user) = Right user
requireAuth NoSuchUser = Left err401 {errBody = "User not found"}
requireAuth BadPassword = Left err403 {errBody = "Invalid password"}
requireAuth Indefinite = Left err401 {errBody = "Authentication required"}

-- | Check basic auth and return the user
checkBasicAuth :: AppEnv -> (ByteString, ByteString) -> IO (AuthResult AuthenticatedUser)
checkBasicAuth = fromBasicAuthData
