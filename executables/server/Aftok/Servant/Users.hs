{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Users
  ( -- * API Types (re-exported from aftok-api)
    UsersAPI,
    ProtectedUsersAPI,
    RegisterRequest (..),
    RegisterError (..),
    UsernameCheckResponse (..),
    ZAddrCheckResponse (..),
    CaptchaError (..),
    CaptchaResponse (..),
    AddressInvalid (..),

    -- * Lenses (re-exported from aftok-api)
    regUser,
    password,
    captchaToken,
    invitationCodes,
    username,
    userAccountRecovery,

    -- * Handlers
    usersServer,
    acceptInvitationHandler,

    -- * Configuration
    RegisterOps (..),
    CaptchaConfig (..),

    -- * Utilities
    checkCaptcha,
  )
where

import Aftok.API.Users
  ( AddressInvalid (..),
    CaptchaError (..),
    CaptchaResponse (..),
    ProtectedUsersAPI,
    RegisterError (..),
    RegisterRequest (..),
    UsernameCheckResponse (..),
    UsersAPI,
    ZAddrCheckResponse (..),
    captchaToken,
    invitationCodes,
    password,
    regUser,
    userAccountRecovery,
    username,
  )
import Aftok.Currency.Zcash (Address (..))
import Aftok.Database
  ( acceptInvitation,
    createUserWithPassword,
    findCurrentInvitation,
    findUserByName,
  )
import Aftok.Password (hashPassword)
import Aftok.Project (parseInvCode)
import Aftok.ServerConfig (CaptchaConfig (..), captchaSecretKey)
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Types
  ( Email (..),
    RecoverBy (..),
    User (..),
    UserId,
    UserName (..),
  )
import Control.Lens ((^.))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Thyme.Clock as C
import Network.HTTP.Client
  ( httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.MultipartFormData
  ( formDataBody,
    partBS,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Servant

--------------------------------------------------------------------------------
-- Server-specific Types
--------------------------------------------------------------------------------

-- | Operations needed for registration
data RegisterOps m = RegisterOps
  { validateZAddr :: Text -> m (Either AddressInvalid Address),
    sendConfirmationEmail :: Email -> m ()
  }

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Check captcha with Google's API
checkCaptcha :: CaptchaConfig -> Text -> IO (Either [CaptchaError] ())
checkCaptcha cfg token = do
  request <- parseRequest "https://www.google.com/recaptcha/api/siteverify"
  reqWithBody <-
    formDataBody
      [ partBS "secret" (T.encodeUtf8 $ cfg ^. captchaSecretKey),
        partBS "response" (T.encodeUtf8 token)
      ]
      request
  manager <- newTlsManager
  response <- httpLbs reqWithBody manager
  pure $ case statusCode (responseStatus response) of
    200 -> case A.eitherDecode (responseBody response) of
      Left err ->
        Left [CaptchaError $ "Failed to decode JSON response: " <> T.pack err]
      Right cr -> if success cr then Right () else Left (errorCodes cr)
    errCode ->
      Left [CaptchaError $ "Unexpected status code: " <> T.pack (show errCode)]

-- | Users server implementation
usersServer ::
  RegisterOps IO ->
  CaptchaConfig ->
  ServerT UsersAPI AppM
usersServer ops cfg =
  checkUsernameHandler
    :<|> checkZAddrHandler ops
    :<|> registerHandler ops cfg

-- | Check if a username is available
checkUsernameHandler :: Maybe Text -> AppM UsernameCheckResponse
checkUsernameHandler Nothing =
  throwError err400 {errBody = "username parameter is required"}
checkUsernameHandler (Just uname) = do
  found <- runDB (runMaybeT $ findUserByName (UserName uname))
  case found of
    Nothing -> pure $ UsernameCheckResponse True Nothing
    Just _ -> pure $ UsernameCheckResponse False (Just "username is already taken")

-- | Check if a Z-address is valid
checkZAddrHandler :: RegisterOps IO -> Maybe Text -> AppM ZAddrCheckResponse
checkZAddrHandler _ Nothing =
  throwError err400 {errBody = "zaddr parameter is required"}
checkZAddrHandler ops (Just zaddr) = do
  result <- liftIO $ validateZAddr ops zaddr
  case result of
    Left _ -> pure $ ZAddrCheckResponse False (Just "Invalid Z-address")
    Right _ -> pure $ ZAddrCheckResponse True Nothing

-- | Register a new user
registerHandler ::
  RegisterOps IO ->
  CaptchaConfig ->
  RegisterRequest ->
  AppM UserId
registerHandler ops cfg req = do
  now <- liftIO C.getCurrentTime
  let regU = req ^. regUser

  -- Check for valid invitation codes
  invResults <- runDB $ traverse (findCurrentInvitation now) (req ^. invitationCodes)
  let (_, validInvs) = partitionEithers invResults

  -- If no valid invitation codes, require captcha
  when (null validInvs) $ do
    case req ^. captchaToken of
      Nothing ->
        throwError err400 {errBody = "Captcha token or invitation code required."}
      Just token -> do
        captchaResult <- liftIO $ checkCaptcha cfg token
        case captchaResult of
          Left errs ->
            throwError err400 {errBody = "Captcha check failed: " <> show errs}
          Right () -> pure ()

  -- Validate account recovery method
  acctRecovery <- case regU ^. userAccountRecovery of
    RecoverByEmail e -> do
      liftIO $ sendConfirmationEmail ops e
      pure $ RecoverByEmail e
    RecoverByZAddr z -> do
      zaddrResult <- liftIO $ validateZAddr ops z
      case zaddrResult of
        Left _ ->
          throwError err400 {errBody = "Invalid Z-address for account recovery"}
        Right r ->
          pure $ RecoverByZAddr r

  -- Hash the password and create the user
  let uname = regU ^. username
  pwdHash <- liftIO $ hashPassword (req ^. password)
  runDB $ do
    userId <- createUserWithPassword (User uname acctRecovery) pwdHash
    void $ traverse (acceptInvitation userId now) (req ^. invitationCodes)
    pure userId

-- | Accept a project invitation (protected endpoint)
acceptInvitationHandler ::
  AuthenticatedUser ->
  [Text] ->
  AppM NoContent
acceptInvitationHandler user invCodeTexts = do
  let uid = auUserId user
  now <- liftIO C.getCurrentTime

  -- Parse invitation codes
  invCodes <- case traverse parseInvCode invCodeTexts of
    Left e ->
      throwError err400 {errBody = "Invalid invitation code: " <> encodeUtf8 e}
    Right codes -> pure codes

  -- Accept each invitation
  runDB $ void $ traverse (acceptInvitation uid now) invCodes
  pure NoContent
