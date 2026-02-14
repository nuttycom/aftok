{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.PasswordReset
  ( -- * API Types (re-exported from aftok-api)
    PasswordResetAPI,
    PasswordResetRequest (..),
    PasswordResetResponse (..),
    PasswordResetConfirm (..),

    -- * Handlers
    passwordResetServer,

    -- * Operations
    PasswordResetOps (..),
  )
where

import Aftok.API.PasswordReset
  ( PasswordResetAPI,
    PasswordResetConfirm (..),
    PasswordResetRequest (..),
    PasswordResetResponse (..),
  )
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
    RecoverBy (..),
    UserName (..),
    prtExpiresAt,
    prtUsedAt,
    prtUserId,
    userAccountRecovery,
    username,
    _UserName,
  )
import Control.Lens ((^.))
import qualified Data.Thyme.Clock as C
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Servant

-- | Operations needed for password reset
data PasswordResetOps m = PasswordResetOps
  { -- | Send password reset email (to email, username, reset URL, expiry hours)
    sendPasswordResetEmail :: Email -> Text -> Text -> Text -> m (),
    -- | Generate the password reset URL from the token
    generateResetUrl :: Text -> Text
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
    :<|> validateTokenHandler
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

-- | Validate a password reset token without consuming it
validateTokenHandler ::
  Text ->
  AppM NoContent
validateTokenHandler tokenText = do
  now <- liftIO C.getCurrentTime
  mTokenInfo <- runDB $ runMaybeT $ findPasswordResetToken tokenText
  case mTokenInfo of
    Nothing ->
      throwError err400 {errBody = "Invalid or expired password reset token"}
    Just (_tokenId, token) ->
      if now > token ^. prtExpiresAt || isJust (token ^. prtUsedAt)
        then throwError err400 {errBody = "Invalid or expired password reset token"}
        else pure NoContent

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
