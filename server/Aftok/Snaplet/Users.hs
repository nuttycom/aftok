{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Aftok.Snaplet.Users
  ( acceptInvitationHandler
  , checkZAddrHandler
  , registerHandler
  , CaptchaConfig(..)
  , CaptchaError(..)
  , checkCaptcha
  , RegisterOps(..)
  )
where



import           Control.Lens
import           Control.FromSum                ( fromMaybeM )
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                )
import qualified Data.Map.Strict               as M
import           Data.Text                     as T
import           Data.Text.Encoding            as T
import           Data.Thyme.Clock              as C

import           Network.HTTP.Client            ( parseRequest
                                                , responseBody
                                                , responseStatus
                                                , httpLbs
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Network.HTTP.Client.MultipartFormData
                                                ( formDataBody
                                                , partBS
                                                )
import           Network.HTTP.Types.Status      ( statusCode )

import           Aftok.Currency.Zcash           ( ZAddr )
import           Aftok.Database                 ( createUser, acceptInvitation )
import           Aftok.Project                  ( InvitationCode, parseInvCode )
import           Aftok.Users                    ( RegisterOps(..) )
import           Aftok.Types                    ( UserId, User(..)
                                                , AccountRecovery(..)
                                                , Email(..)
                                                , UserName(..), _UserName
                                                )

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import qualified Snap.Core                     as S
import qualified Snap.Snaplet                  as S
import qualified Snap.Snaplet.Auth             as AU

data RegUser = RegUser
  { _username             :: !UserName
  , _userAccountRecovery  :: !(AccountRecovery Text)
  }
makeLenses ''RegUser

data RegisterRequest = RegisterRequest
  { _regUser         :: RegUser
  , _password        :: ByteString
  , _captchaToken    :: Text
  , _invitationCodes :: [InvitationCode]
  }
makeLenses ''RegisterRequest

instance A.FromJSON RegisterRequest where
  parseJSON (A.Object v) = do
    recoveryType <- v .: "recoveryType"
    recovery <- case (recoveryType :: Text) of
      "email" -> RecoverByEmail . Email <$> v .: "recoveryEmail"
      "zaddr" -> RecoverByZAddr         <$> v .: "recoveryZAddr"
      _ -> Prelude.empty
    user <- RegUser <$> (UserName <$> v .: "username")
                    <*> pure recovery

    RegisterRequest user
      <$> (fromString <$> v .: "password")
      <*> (v .: "captchaToken")
      <*> (parseInvitationCodes =<< v .: "invitation_codes")

    where
      parseInvitationCodes c = either
             (\e -> fail $ "Invitation code was rejected as invalid: " <> e)
             pure
             (traverse parseInvCode c)
  parseJSON _ = mzero

checkZAddrHandler :: RegisterOps IO -> S.Handler App App ZAddr
checkZAddrHandler ops = do
  params <- S.getParams
  zaddrBytes <- maybe (snapError 400 "zaddr parameter is required")
                       pure
                       (listToMaybe =<< M.lookup "zaddr" params)
  zaddrEither <- liftIO $ parseZAddr ops (T.decodeUtf8 zaddrBytes)
  case zaddrEither of
    Left _  ->
      snapError 400 "The Z-Address provided for account recovery was invalid."
    Right zaddr ->
      pure zaddr



registerHandler :: RegisterOps IO -> CaptchaConfig -> S.Handler App App UserId
registerHandler ops cfg = do
  rbody    <- S.readRequestBody 4096
  userData <- fromMaybeM (snapError 400 "Could not parse user data") (A.decode rbody)

  captchaResult <- liftIO $ checkCaptcha cfg (userData ^. captchaToken)
  let captchaFailed = throwDenied $ AU.AuthError "Captcha check failed, please try again."
  void . either (const captchaFailed) pure $ captchaResult

  acctRecovery <- case (userData ^. regUser . userAccountRecovery) of
    RecoverByEmail e -> do
      liftIO $ sendConfirmationEmail ops e
      pure $ RecoverByEmail e
    RecoverByZAddr z -> do
      zaddrValid <- liftIO $ parseZAddr ops z
      case zaddrValid of
        Left _  ->
          snapError 400 "The Z-Address provided for account recovery was invalid."
        Right r ->
          pure $ RecoverByZAddr r

  now <- liftIO C.getCurrentTime
  let
    uname = userData ^. (regUser . username)
    createSUser = AU.createUser (uname ^. _UserName) (userData ^. password)
    createQUser = snapEval $ do
      userId <- createUser $ User uname acctRecovery
      void $ traverse (acceptInvitation userId now) (userData ^. invitationCodes)
      pure userId
  authUser <- S.with auth createSUser
  either throwDenied (\_ -> createQUser) authUser


acceptInvitationHandler :: S.Handler App App ()
acceptInvitationHandler = do
  uid      <- requireUserId
  now      <- liftIO C.getCurrentTime
  params   <- S.getParams
  invCodes <- maybe (snapError 400 "invCode parameter is required")
                    (pure . traverse (parseInvCode . T.decodeUtf8))
                    (M.lookup "invCode" params)
  either
    (\e ->
      snapError 400 $ "Invitation code was rejected as invalid: " <> T.pack e
    )
    (\cx -> void . snapEval $ traverse (acceptInvitation uid now) cx)
    invCodes

type CaptchaCheckResult = Either [CaptchaError] ()

data CaptchaError
  = MissingInputSecret
  | InvalidInputSecret
  | MissingInputResponse
  | InvalidInputResponse
  | BadRequest
  | TimeoutOrDuplicate
  | CaptchaError Text
  deriving (Eq, Show)

data CaptchaConfig = CaptchaConfig
  { secretKey :: Text }

data CaptchaResponse = CaptchaResponse
  { success :: Bool
  , errorCodes :: [CaptchaError]
  }

instance A.FromJSON CaptchaResponse where
  parseJSON (A.Object v) =
    CaptchaResponse
      <$> v
      .:  "success"
      <*> (fmap toError . join . toList <$> v .:? "error-codes")
   where
    toError = \case
      "missing-input-secret"   -> MissingInputSecret
      "invalid-input-secret"   -> InvalidInputSecret
      "missing-input-response" -> MissingInputResponse
      "invalid-input-response" -> InvalidInputResponse
      "bad-request"            -> BadRequest
      "timeout-or-duplicate"   -> TimeoutOrDuplicate
      other -> CaptchaError $ "Unexpected error code: " <> other
  parseJSON _ = fail "Captcha response body was not a valid JSON object."

checkCaptcha :: CaptchaConfig -> Text -> IO CaptchaCheckResult
checkCaptcha cfg token = do
  request     <- parseRequest "https://www.google.com/recaptcha/api/siteverify"
  reqWithBody <- formDataBody
    [ partBS "secret"   (T.encodeUtf8 $ secretKey cfg)
    , partBS "response" (T.encodeUtf8 token)
    ]
    request
  manager  <- newTlsManager
  response <- httpLbs reqWithBody manager
  pure $ case statusCode (responseStatus response) of
    200 -> case A.eitherDecode (responseBody response) of
      Left err ->
        Left [CaptchaError $ "Failed to decode JSON response: " <> T.pack err]
      Right cr -> if success cr then Right () else Left (errorCodes cr)
    errCode ->
      Left
        $ [CaptchaError $ "Unexpected status code: " <> T.pack (show errCode)]
