{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Users
  ( -- * API Types
    UsersAPI,

    -- * Handlers
    usersServer,

    -- * Request/Response Types
    RegisterRequest (..),
    RegisterError (..),
    UsernameCheckResponse (..),
    ZAddrCheckResponse (..),

    -- * Configuration
    RegisterOps (..),
    CaptchaConfig (..),
    CaptchaError (..),
    AddressInvalid (..),

    -- * Utilities
    checkCaptcha,
  )
where

import Aftok.Currency.Zcash (Address (..))
import Aftok.Database
  ( acceptInvitation,
    createUser,
    findCurrentInvitation,
    findUserByName,
  )
import Aftok.Project (InvitationCode, parseInvCode)
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Types
  ( Email (..),
    RecoverBy (..),
    User (..),
    UserId,
    UserName (..),
  )
import Control.Lens (makeLenses, (^.))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    (.:),
    (.:?),
    (.=),
  )
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
import Servant.Auth.Server (AuthResult (..))

-- | Users API type
type UsersAPI =
  -- GET /validate_username?username=...
  "validate_username"
    :> QueryParam "username" Text
    :> Get '[JSON] UsernameCheckResponse
    -- GET /validate_zaddr?zaddr=...
    :<|> "validate_zaddr"
      :> QueryParam "zaddr" Text
      :> Get '[JSON] ZAddrCheckResponse
    -- POST /register
    :<|> "register"
      :> ReqBody '[JSON] RegisterRequest
      :> Post '[JSON] UserId

-- | Protected Users API (requires authentication)
type ProtectedUsersAPI =
  -- POST /accept_invitation?invCode=...
  "accept_invitation"
    :> QueryParams "invCode" Text
    :> Post '[JSON] NoContent

-- | Address validation error
data AddressInvalid = AddressInvalid
  deriving (Show, Eq)

-- | Operations needed for registration
data RegisterOps m = RegisterOps
  { validateZAddr :: Text -> m (Either AddressInvalid Address),
    sendConfirmationEmail :: Email -> m ()
  }

-- | User data for registration
data RegUser = RegUser
  { _username :: !UserName,
    _userAccountRecovery :: !(RecoverBy Text)
  }
  deriving (Show, Eq)

makeLenses ''RegUser

-- | Registration request
data RegisterRequest = RegisterRequest
  { _regUser :: RegUser,
    _password :: ByteString,
    _captchaToken :: Maybe Text,
    _invitationCodes :: [InvitationCode]
  }

makeLenses ''RegisterRequest

instance FromJSON RegisterRequest where
  parseJSON (A.Object v) = do
    recoveryType <- v .: "recoveryType"
    recovery <- case (recoveryType :: Text) of
      "email" -> RecoverByEmail . Email <$> v .: "recoveryEmail"
      "zaddr" -> RecoverByZAddr <$> v .: "recoveryZAddr"
      _ -> Prelude.empty
    user <-
      RegUser
        <$> (UserName <$> v .: "username")
        <*> pure recovery
    RegisterRequest user
      <$> (fromString <$> v .: "password")
      <*> (v .:? "captchaToken")
      <*> (parseInvitationCodes . join . maybeToList =<< v .:? "invitation_codes")
    where
      parseInvitationCodes c =
        either
          (\e -> fail $ "Invitation code was rejected as invalid: " <> toString e)
          pure
          (traverse parseInvCode c)
  parseJSON _ = mzero

-- | Registration error
data RegisterError
  = RegParseError String
  | RegCaptchaError [CaptchaError]
  | RegZAddrError AddressInvalid
  deriving (Show)

instance ToJSON RegisterError where
  toJSON = \case
    RegParseError msg ->
      A.object ["parseError" .= msg]
    RegCaptchaError e ->
      A.object ["captchaError" .= (show e :: Text)]
    RegZAddrError zerr ->
      A.object ["zaddrError" .= (show zerr :: Text)]

-- | Username check response
data UsernameCheckResponse = UsernameCheckResponse
  { usernameAvailable :: Bool,
    usernameMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UsernameCheckResponse

-- | Z-address check response
data ZAddrCheckResponse = ZAddrCheckResponse
  { zaddrValid :: Bool,
    zaddrMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ZAddrCheckResponse

-- | Captcha configuration
newtype CaptchaConfig = CaptchaConfig
  { secretKey :: Text
  }

-- | Captcha errors
data CaptchaError
  = MissingInputSecret
  | InvalidInputSecret
  | MissingInputResponse
  | InvalidInputResponse
  | BadRequest
  | TimeoutOrDuplicate
  | CaptchaError Text
  deriving (Eq, Show)

-- | Captcha response from Google
data CaptchaResponse = CaptchaResponse
  { success :: Bool,
    errorCodes :: [CaptchaError]
  }

instance FromJSON CaptchaResponse where
  parseJSON (A.Object v) =
    CaptchaResponse
      <$> v .: "success"
      <*> (fmap toError . join . toList <$> v .:? "error-codes")
    where
      toError = \case
        "missing-input-secret" -> MissingInputSecret
        "invalid-input-secret" -> InvalidInputSecret
        "missing-input-response" -> MissingInputResponse
        "invalid-input-response" -> InvalidInputResponse
        "bad-request" -> BadRequest
        "timeout-or-duplicate" -> TimeoutOrDuplicate
        other -> CaptchaError $ "Unexpected error code: " <> other
  parseJSON _ = fail "Captcha response body was not a valid JSON object."

-- | Check captcha with Google's API
checkCaptcha :: CaptchaConfig -> Text -> IO (Either [CaptchaError] ())
checkCaptcha cfg token = do
  request <- parseRequest "https://www.google.com/recaptcha/api/siteverify"
  reqWithBody <-
    formDataBody
      [ partBS "secret" (T.encodeUtf8 $ secretKey cfg),
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
  acctRecovery <- case req ^. regUser . userAccountRecovery of
    RecoverByEmail e -> do
      liftIO $ sendConfirmationEmail ops e
      pure $ RecoverByEmail e
    RecoverByZAddr z -> do
      zaddrValid <- liftIO $ validateZAddr ops z
      case zaddrValid of
        Left _ ->
          throwError err400 {errBody = "Invalid Z-address for account recovery"}
        Right r ->
          pure $ RecoverByZAddr r

  -- Create the user
  -- Note: In the Snap version, this also creates an AU.AuthUser.
  -- For Servant, we'll need to handle password storage differently.
  -- For now, we create only the domain user.
  let uname = req ^. regUser . username
  runDB $ do
    userId <- createUser $ User uname acctRecovery
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
