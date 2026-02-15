{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson (FromJSON (..), ToJSON (..))
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

instance HasCodec PasswordResetRequest where
  codec =
    object "PasswordResetRequest" $
      PasswordResetRequest
        <$> optionalField' "username" AC..= prrUsername
        <*> optionalField' "email" AC..= prrEmail

instance FromJSON PasswordResetRequest where parseJSON = parseJSONViaCodec

-- | Response for password reset request (always success for security)
data PasswordResetResponse = PasswordResetResponse
  { prsMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec PasswordResetResponse where
  codec =
    object "PasswordResetResponse" $
      PasswordResetResponse
        <$> requiredField' "message" AC..= prsMessage

instance ToJSON PasswordResetResponse where toJSON = toJSONViaCodec

-- | Confirm password reset with token
data PasswordResetConfirm = PasswordResetConfirm
  { prcToken :: Text,
    prcNewPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec PasswordResetConfirm where
  codec =
    object "PasswordResetConfirm" $
      PasswordResetConfirm
        <$> requiredField' "token" AC..= prcToken
        <*> requiredField' "newPassword" AC..= prcNewPassword

instance FromJSON PasswordResetConfirm where parseJSON = parseJSONViaCodec
