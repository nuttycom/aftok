{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.PasswordReset
  ( -- * API Types
    PasswordResetAPI,

    -- * Handlers
    passwordResetServer,

    -- * Request/Response Types
    PasswordResetRequest (..),
    PasswordResetConfirm (..),

    -- * Operations
    PasswordResetOps (..),
  )
where

import Aftok.Database
  ( createPasswordResetToken,
    findPasswordResetToken,
    findUserByEmail,
    findUserByName,
    markPasswordResetTokenUsed,
    updateUserPassword,
  )
import Aftok.Password (hashPassword)
import Aftok.Servant.App (AppM, runDB)
import Aftok.Types
  ( Email (..),
    UserName (..),
    RecoverBy (..),
    userAccountRecovery,
    username,
    _UserName,
    prtExpiresAt,
    prtUsedAt,
    prtUserId,
  )
import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Thyme.Clock as C
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Servant

-- | Password reset API
type PasswordResetAPI =
  -- POST /password-reset/request - request a password reset
  "password-reset"
    :> "request"
    :> ReqBody '[JSON] PasswordResetRequest
    :> Post '[JSON] PasswordResetResponse
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

-- | Operations needed for password reset
data PasswordResetOps m = PasswordResetOps
  { sendPasswordResetEmail :: Email -> Text -> Text -> Text -> m (),
    -- ^ Send password reset email (to email, username, reset URL, expiry hours)
    generateResetUrl :: Text -> Text
    -- ^ Generate the password reset URL from the token
  }

-- Token validity period in seconds (24 hours)
tokenValiditySeconds :: Int
tokenValiditySeconds = 24 * 60 * 60

-- | Password reset server implementation
passwordResetServer ::
  PasswordResetOps IO ->
  ServerT PasswordResetAPI AppM
passwordResetServer ops =
  requestPasswordResetHandler ops
    :<|> confirmPasswordResetHandler

-- | Handle password reset request
requestPasswordResetHandler ::
  PasswordResetOps IO ->
  PasswordResetRequest ->
  AppM PasswordResetResponse
requestPasswordResetHandler ops req = do
  -- Always return success for security (don't reveal if user exists)
  let successMsg = "If an account with that username or email exists, a password reset link has been sent."

  -- Try to find the user by username or email
  mUserInfo <- case (prrUsername req, prrEmail req) of
    (Just uname, _) -> runDB $ runMaybeT $ findUserByName (UserName uname)
    (_, Just email) -> runDB $ runMaybeT $ findUserByEmail (Email email)
    (Nothing, Nothing) -> pure Nothing

  case mUserInfo of
    Nothing ->
      -- User not found, but still return success for security
      pure $ PasswordResetResponse successMsg
    Just (uid, user) -> do
      -- Check if user has an email for recovery
      case user ^. userAccountRecovery of
        RecoverByEmail email -> do
          -- Generate token
          token <- liftIO $ UUID.toText <$> UUID.nextRandom
          now <- liftIO C.getCurrentTime
          let expiresAt = addSeconds tokenValiditySeconds now

          -- Store token in database
          _ <- runDB $ createPasswordResetToken uid token expiresAt

          -- Send email
          let resetUrl = generateResetUrl ops token
              uname = user ^. username . _UserName
          liftIO $ sendPasswordResetEmail ops email uname resetUrl "24"

          pure $ PasswordResetResponse successMsg

        RecoverByZAddr _ ->
          -- User has Zcash address for recovery, not email
          -- For now, we can't send password reset via Zcash
          -- Return success anyway for security
          pure $ PasswordResetResponse successMsg

-- | Handle password reset confirmation
confirmPasswordResetHandler ::
  PasswordResetConfirm ->
  AppM NoContent
confirmPasswordResetHandler req = do
  now <- liftIO C.getCurrentTime

  -- Find the token
  mTokenInfo <- runDB $ runMaybeT $ findPasswordResetToken (prcToken req)

  case mTokenInfo of
    Nothing ->
      throwError err400 {errBody = "Invalid or expired password reset token"}
    Just (tokenId, token) -> do
      -- Check if token is expired
      if now > token ^. prtExpiresAt
        then throwError err400 {errBody = "Invalid or expired password reset token"}
        else
          -- Check if token is already used
          if isJust (token ^. prtUsedAt)
            then throwError err400 {errBody = "Invalid or expired password reset token"}
            else do
              -- Hash the new password
              pwdHash <- liftIO $ hashPassword (encodeUtf8 $ prcNewPassword req)

              -- Update password and mark token as used
              runDB $ do
                updateUserPassword (token ^. prtUserId) pwdHash
                markPasswordResetTokenUsed tokenId now

              pure NoContent

-- | Add seconds to a UTCTime
addSeconds :: Int -> C.UTCTime -> C.UTCTime
addSeconds secs time =
  let diff = fromIntegral secs :: C.NominalDiffTime
   in C.addUTCTime diff time
