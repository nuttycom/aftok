{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Authentication types for the Aftok API.
module Aftok.API.Auth
  ( -- * Types
    AuthenticatedUser (..),
    LoginRequest (..),

    -- * Auth combinators
    AftokAuth,
  )
where

import Aftok.Types (UserId (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import Servant.Auth (Auth, BasicAuth, Cookie, JWT)
import Servant.Auth.Server (FromJWT, ToJWT)

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
