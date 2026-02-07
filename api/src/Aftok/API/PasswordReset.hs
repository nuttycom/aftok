{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Password Reset API types for the Aftok API.
module Aftok.API.PasswordReset
  ( -- * API Types
    PasswordResetAPI,

    -- * Request/Response Types
    PasswordResetRequest (..),
    PasswordResetResponse (..),
    PasswordResetConfirm (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import Servant.API

-- | Password reset API
type PasswordResetAPI =
  -- POST /password-reset/request - request a password reset
  "password-reset"
    :> "request"
    :> ReqBody '[JSON] PasswordResetRequest
    :> Post '[JSON] PasswordResetResponse
    -- GET /password-reset/validate/<token> - check if a reset token is valid
    :<|> "password-reset"
      :> "validate"
      :> Capture "token" Text
      :> Get '[JSON] NoContent
    -- POST /password-reset/reset - confirm password reset with token
    :<|> "password-reset"
      :> "reset"
      :> ReqBody '[JSON] PasswordResetConfirm
      :> Post '[JSON] NoContent

-- | Request for password reset
data PasswordResetRequest = PasswordResetRequest
  { prrUsername :: Maybe Text,
    prrEmail :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PasswordResetRequest where
  parseJSON = A.withObject "PasswordResetRequest" $ \o ->
    PasswordResetRequest
      <$> o .:? "username"
      <*> o .:? "email"
    where
      (.:?) obj key = obj .: key <|> pure Nothing

-- | Response for password reset request (always success for security)
data PasswordResetResponse = PasswordResetResponse
  { prsMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PasswordResetResponse where
  toJSON (PasswordResetResponse msg) =
    A.object ["message" .= msg]

-- | Confirm password reset with token
data PasswordResetConfirm = PasswordResetConfirm
  { prcToken :: Text,
    prcNewPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PasswordResetConfirm where
  parseJSON = A.withObject "PasswordResetConfirm" $ \o ->
    PasswordResetConfirm
      <$> o .: "token"
      <*> o .: "newPassword"
