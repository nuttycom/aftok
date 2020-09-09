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
import qualified Data.Aeson                    as A
import           Data.Aeson                    ((.:), (.:?))
import qualified Data.Map.Strict               as M
import           Data.Text                     as T
import           Data.Text.Encoding            as T
import           Data.Thyme.Clock              as C

import           Haskoin.Address                ( textToAddr )
import           Network.HTTP.Client            ( parseRequest, responseBody, responseStatus, httpLbs)
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import           Network.HTTP.Types.Status      ( statusCode )

import           Aftok.Types
import           Aftok.Currency.Bitcoin         ( NetworkId(..)
                                                , toNetwork
                                                )
import           Aftok.Database
import           Aftok.Project
import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import qualified  Snap.Core                    as S
import qualified  Snap.Snaplet                 as S
import qualified Snap.Snaplet.Auth             as AU

data CUser = CU
  { _cuser           :: User Text
  , _password        :: ByteString
  , _invitationCodes :: [InvitationCode]
  }
makeLenses ''CUser

instance A.FromJSON CUser where
  parseJSON (A.Object v) =
    let parseUser =
            User
              <$> (UserName <$> v .: "username")
              <*> (v .: "btcAddr")
              <*> (Email <$> v .: "email")

        parseInvitationCodes c = either
          (\e -> fail $ "Invitation code was rejected as invalid: " <> e)
          pure
          (traverse parseInvCode c)
    in  CU
          <$> parseUser
          <*> (fromString <$> v .: "password")
          <*> (parseInvitationCodes =<< v .: "invitation_codes")

  parseJSON _ = mzero

registerHandler :: S.Handler App App UserId
registerHandler = do
  rbody <- S.readRequestBody 4096
  -- allow any number of 'invitationCode' query parameters
  userData    <- maybe (snapError 400 "Could not parse user data") pure
    $ A.decode rbody
  t     <- liftIO C.getCurrentTime
  nmode <- getNetworkMode
  let addr =
        textToAddr (toNetwork nmode BTC) =<< (userData ^. cuser . userAddress)
  let
    createSUser = AU.createUser (userData ^. (cuser . username . _UserName))
                                (userData ^. password)
    createQUser = snapEval $ do
      userId <- createUser
        ((userData ^. cuser) & userAddress .~ ((BTC, ) <$> addr))
      void $ traverse (acceptInvitation userId t) (userData ^. invitationCodes)
      return userId
  authUser <- S.with auth createSUser
  either throwDenied (\_ -> createQUser) authUser

acceptInvitationHandler :: S.Handler App App ()
acceptInvitationHandler = do
  uid      <- requireUserId
  t        <- liftIO C.getCurrentTime
  params   <- S.getParams
  invCodes <- maybe (snapError 400 "invCode parameter is required")
                    (pure . traverse (parseInvCode . T.decodeUtf8))
                    (M.lookup "invCode" params)
  either
    (\e ->
      snapError 400 $ "Invitation code was rejected as invalid: " <> T.pack e
    )
    (\cx -> void . snapEval $ traverse (acceptInvitation uid t) cx)
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
    CaptchaResponse <$> v .: "success"
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
  parseJSON _ =
    fail "Captcha response body was not a valid JSON object."

checkCaptcha :: CaptchaConfig -> Text -> IO CaptchaCheckResult
checkCaptcha cfg token = do
  request <- parseRequest "https://www.google.com/recaptcha/api/siteverify"
  reqWithBody <- formDataBody [partBS "secret" (T.encodeUtf8 $ secretKey cfg), partBS "response" (T.encodeUtf8 token)] request
  manager <- newTlsManager
  response <- httpLbs reqWithBody manager
  pure $ case statusCode (responseStatus response) of
    200 ->
      case A.eitherDecode (responseBody response) of
        Left err -> Left [CaptchaError $ "Failed to decode JSON response: " <> T.pack err]
        Right cr -> if success cr then Right () else Left (errorCodes cr)
    errCode ->
      Left $ [CaptchaError $ "Unexpected status code: " <> T.pack (show errCode)]
