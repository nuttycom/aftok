{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Aftok.Snaplet.Users
  ( registerHandler
  , acceptInvitationHandler
  , CaptchaConfig(..)
  , CaptchaError(..)
  , checkCaptcha
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

import           Aftok.Currency.ZCash           ( ZAddr(..) )
import           Aftok.Database
import           Aftok.Project
import           Aftok.Types

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import qualified Snap.Core                     as S
import qualified Snap.Snaplet                  as S
import qualified Snap.Snaplet.Auth             as AU

data RegisterRequest = RegisterRequest
  { _cuser           :: User
  , _password        :: ByteString
  , _captchaToken    :: Text
  , _invitationCodes :: [InvitationCode]
  }
makeLenses ''RegisterRequest

instance A.FromJSON RegisterRequest where
  parseJSON (A.Object v) = do
    recoveryType <- v .: "recoveryType"
    recovery <- case (recoveryType :: Text) of
      "email" -> RecoverByEmail . Email <$> v .: "email"
      "zaddr" -> RecoverByZAddr . ZAddr <$> v .: "zaddr"
      _ -> Prelude.empty
    user <- User <$> (UserName <$> v .: "username")
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

registerHandler :: CaptchaConfig -> S.Handler App App UserId
registerHandler cfg = do
  rbody    <- S.readRequestBody 4096
  userData <- fromMaybeM (snapError 400 "Could not parse user data") (A.decode rbody)
  captchaResult <- liftIO $ checkCaptcha cfg (userData ^. captchaToken)
  void . either (const . throwDenied $ AU.AuthError "Captcha check failed, please try again.") pure $ captchaResult

  now   <- liftIO C.getCurrentTime
  let
    createSUser = AU.createUser (userData ^. (cuser . username . _UserName))
                                (userData ^. password)
    createQUser = snapEval $ do
      userId <- createUser (userData ^. cuser)
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
