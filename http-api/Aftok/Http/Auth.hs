{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Http.Auth where

import Aftok.Currency.Zcash (RPCError, ZValidateAddressErr)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database
  ( InvitationError,
  )
import Aftok.Project (Invitation, InvitationCode, parseInvCode)
import Aftok.Time (Clock, getCurrentTime)
import Aftok.Types
  ( Email (..),
    RecoverBy (..),
    User (..),
    UserId,
    UserName (..),
  )
import Control.Lens (makeLenses, (^.))
import Control.Monad.Trans.Except (throwE)
import Crypto.KDF.BCrypt (hashPassword)
import Crypto.Random.Types (MonadRandom)
import Data.Aeson
  ( (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C
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

data AuthConfig = AuthConfig
  { _hashingCost :: Int
  }

makeLenses ''AuthConfig

data CaptchaError
  = MissingInputSecret
  | InvalidInputSecret
  | MissingInputResponse
  | InvalidInputResponse
  | BadRequest
  | TimeoutOrDuplicate
  | CaptchaError Text
  deriving (Eq, Show)

type CaptchaCheckResult = Either [CaptchaError] ()

data RegisterCaps m = RegisterCaps
  { validateZAddr :: Text -> m (Either (RPCError ZValidateAddressErr) Zcash.Address),
    sendConfirmationEmail :: Email -> m (),
    checkCaptcha :: Text -> m CaptchaCheckResult,
    findCurrentInvitation :: C.UTCTime -> InvitationCode -> m (Either InvitationError Invitation),
    createUser :: User -> m UserId,
    acceptInvitation :: UserId -> C.UTCTime -> InvitationCode -> m ()
  }

data RegUser = RegUser
  { _username :: !UserName,
    _userAccountRecovery :: !(RecoverBy Text)
  }

makeLenses ''RegUser

data RegisterRequest = RegisterRequest
  { _regUser :: RegUser,
    _password :: ByteString,
    _captchaToken :: Maybe Text,
    _invitationCodes :: [InvitationCode]
  }

makeLenses ''RegisterRequest

instance A.FromJSON RegisterRequest where
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

data RegisterError
  = RegParseError String
  | RegCaptchaRequired
  | RegCaptchaError [CaptchaError]
  | RegZAddrError (RPCError ZValidateAddressErr)
  deriving (Show)

instance A.ToJSON RegisterError where
  toJSON = \case
    RegParseError msg ->
      A.object
        ["parseError" .= msg]
    RegCaptchaRequired ->
      A.object
        ["captchaError" .= ("Captcha solution or invitation code required." :: Text)]
    RegCaptchaError e ->
      A.object
        ["captchaError" .= (show e :: Text)]
    RegZAddrError zerr ->
      A.object
        ["zaddrError" .= (show zerr :: Text)]

register ::
  MonadRandom m =>
  Clock m ->
  RegisterCaps m ->
  AuthConfig ->
  RegisterRequest ->
  ExceptT RegisterError m UserId
register clock caps cfg ucr = do
  -- TODO: check for duplicate user information

  -- Find any outstanding invitation; a valid invitation code will allow the user
  -- to skip the captcha check.
  now <- lift $ getCurrentTime clock
  (_, invs) <- partitionEithers <$> (lift $ traverse (findCurrentInvitation caps now) (ucr ^. invitationCodes))
  if null invs
    then case (ucr ^. captchaToken) of
      Just ct -> do
        captchaResult <- lift $ checkCaptcha caps ct
        case captchaResult of
          Left err -> throwE (RegCaptchaError err)
          Right _ -> pure ()
      Nothing ->
        throwE RegCaptchaRequired
    else pure ()
  -- Parse account recovery information from the user's request.
  acctRecovery <- case (ucr ^. regUser . userAccountRecovery) of
    -- If the user specified email-based recovery, send an account confirmation email.
    RecoverByEmail e -> do
      lift $ sendConfirmationEmail caps e
      pure $ RecoverByEmail e
    -- If the user specified zcash-address based recovery, we'll need to initiate the
    -- Zcash confirmation flow.
    RecoverByZAddr z -> do
      zaddrValid <- lift $ validateZAddr caps z
      case zaddrValid of
        Left err -> throwE $ RegZAddrError err
        Right r -> pure $ RecoverByZAddr r
  let uname = ucr ^. (regUser . username)
  passwordHash <- lift $ hashPassword (cfg ^. hashingCost) (ucr ^. password)
  userId <- lift . createUser caps $ User uname acctRecovery passwordHash
  lift $ traverse_ (acceptInvitation caps userId now) (ucr ^. invitationCodes)
  pure userId

data CaptchaConfig = CaptchaConfig
  {secretKey :: Text}

data CaptchaResponse = CaptchaResponse
  { success :: Bool,
    errorCodes :: [CaptchaError]
  }

instance A.FromJSON CaptchaResponse where
  parseJSON (A.Object v) =
    CaptchaResponse
      <$> v
        .: "success"
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

checkReCaptcha :: CaptchaConfig -> Text -> IO CaptchaCheckResult
checkReCaptcha cfg token = do
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
      Left $
        [CaptchaError $ "Unexpected status code: " <> T.pack (show errCode)]
