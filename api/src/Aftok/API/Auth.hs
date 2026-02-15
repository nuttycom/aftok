{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Authentication types for the Aftok API.
module Aftok.API.Auth
  ( -- * Types
    AuthenticatedUser (..),
    LoginRequest (..),

    -- * Auth combinators
    AftokAuth,
  )
where

import Aftok.API.Codec ()
import Aftok.Types (UserId (..))
import Autodocodec (HasCodec (..), object, requiredField', (.=))
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Servant.Auth (Auth, BasicAuth, Cookie, JWT)
import Servant.Auth.Server (FromJWT, ToJWT)

-- | Authenticated user info carried in requests
data AuthenticatedUser = AuthenticatedUser
  { auUserId :: !UserId,
    auUsername :: !Text
  }
  deriving (Eq, Show, Generic)

instance HasCodec AuthenticatedUser where
  codec =
    object "AuthenticatedUser" $
      AuthenticatedUser
        <$> requiredField' "userId" .= auUserId
        <*> requiredField' "username" .= auUsername

instance ToJSON AuthenticatedUser where toJSON = toJSONViaCodec

instance FromJSON AuthenticatedUser where parseJSON = parseJSONViaCodec

-- For JWT/cookie auth
instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

-- | Login request body
data LoginRequest = LoginRequest
  { loginUser :: !Text,
    loginPass :: !Text
  }
  deriving (Eq, Show, Generic)

instance HasCodec LoginRequest where
  codec =
    object "LoginRequest" $
      LoginRequest
        <$> requiredField' "username" .= loginUser
        <*> requiredField' "password" .= loginPass

instance FromJSON LoginRequest where parseJSON = parseJSONViaCodec

-- | Auth configuration type for servant-auth
type AftokAuth = Auth '[BasicAuth, Cookie, JWT] AuthenticatedUser
