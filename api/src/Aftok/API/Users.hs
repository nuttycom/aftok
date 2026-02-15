{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Users API types for the Aftok API.
module Aftok.API.Users
  ( -- * API Types
    UsersAPI,
    ProtectedUsersAPI,

    -- * Request/Response Types
    RegisterRequest (..),
    RegisterResponse (..),
    RegUser (..),
    RegisterError (..),
    UsernameCheckResponse (..),
    ZAddrCheckResponse (..),
    AccountSettingsResponse (..),
    SetPaymentAddressRequest (..),

    -- * Captcha Types
    CaptchaError (..),
    CaptchaResponse (..),
    AddressInvalid (..),

    -- * Lenses
    regUser,
    password,
    captchaToken,
    invitationCodes,
    username,
    userAccountRecovery,
  )
where

import Aftok.API.Codec ()
import Aftok.API.Types ()
import Aftok.Project (InvitationCode, parseInvCode)
import Aftok.Types
  ( Email (..),
    RecoverBy (..),
    UserId,
    UserName (..),
  )
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    (.:),
    (.:?),
  )
import qualified Data.Aeson as A
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Registration response
data RegisterResponse = RegisterResponse
  { rrUserId :: UserId
  }
  deriving (Generic)

instance HasCodec RegisterResponse where
  codec =
    object "RegisterResponse" $
      RegisterResponse
        <$> requiredField' "userId" AC..= rrUserId

instance ToJSON RegisterResponse where toJSON = toJSONViaCodec

-- | Address validation error
data AddressInvalid = AddressInvalid
  deriving (Show, Eq)

-- | User data for registration
data RegUser = RegUser
  { _username :: !UserName,
    _userAccountRecovery :: !(RecoverBy Text)
  }

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
      A.object ["parseError" A..= msg]
    RegCaptchaError e ->
      A.object ["captchaError" A..= (show e :: Text)]
    RegZAddrError zerr ->
      A.object ["zaddrError" A..= (show zerr :: Text)]

-- | Username check response
data UsernameCheckResponse = UsernameCheckResponse
  { usernameAvailable :: Bool,
    usernameMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec UsernameCheckResponse where
  codec =
    object "UsernameCheckResponse" $
      UsernameCheckResponse
        <$> requiredField' "usernameAvailable" AC..= usernameAvailable
        <*> optionalField' "usernameMessage" AC..= usernameMessage

instance ToJSON UsernameCheckResponse where toJSON = toJSONViaCodec

-- | Z-address check response
data ZAddrCheckResponse = ZAddrCheckResponse
  { zaddrValid :: Bool,
    zaddrMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec ZAddrCheckResponse where
  codec =
    object "ZAddrCheckResponse" $
      ZAddrCheckResponse
        <$> requiredField' "zaddrValid" AC..= zaddrValid
        <*> optionalField' "zaddrMessage" AC..= zaddrMessage

instance ToJSON ZAddrCheckResponse where toJSON = toJSONViaCodec

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

-- | Account settings response
data AccountSettingsResponse = AccountSettingsResponse
  { asrUsername :: Text,
    asrZcashAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec AccountSettingsResponse where
  codec =
    object "AccountSettingsResponse" $
      AccountSettingsResponse
        <$> requiredField' "username" AC..= asrUsername
        <*> optionalField' "zcashAddress" AC..= asrZcashAddress

instance ToJSON AccountSettingsResponse where toJSON = toJSONViaCodec

-- | Set payment address request
data SetPaymentAddressRequest = SetPaymentAddressRequest
  { sparZcashAddress :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec SetPaymentAddressRequest where
  codec =
    object "SetPaymentAddressRequest" $
      SetPaymentAddressRequest
        <$> requiredField' "zcashAddress" AC..= sparZcashAddress

instance FromJSON SetPaymentAddressRequest where parseJSON = parseJSONViaCodec

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Users API type
type UsersAPI =
  -- GET /check_username?username=...
  "check_username"
    :> QueryParam "username" Text
    :> Get '[JSON] UsernameCheckResponse
    -- GET /validate_zaddr?zaddr=...
    :<|> "validate_zaddr"
      :> QueryParam "zaddr" Text
      :> Get '[JSON] ZAddrCheckResponse
    -- POST /register
    :<|> "register"
      :> ReqBody '[JSON] RegisterRequest
      :> Post '[JSON] RegisterResponse

-- | Protected Users API (requires authentication)
type ProtectedUsersAPI =
  -- POST /accept_invitation?invCode=...
  "accept_invitation"
    :> QueryParams "invCode" Text
    :> Post '[JSON] NoContent
    -- GET /settings
    :<|> "settings"
      :> Get '[JSON] AccountSettingsResponse
    -- PUT /settings/payment-address
    :<|> "settings"
      :> "payment-address"
      :> ReqBody '[JSON] SetPaymentAddressRequest
      :> Put '[JSON] NoContent
