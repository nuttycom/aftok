module Aftok.Api.Account where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Affjax (post, get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

type LoginRequest
  = { username :: String, password :: String }

data LoginResponse
  = LoginOK
  | LoginForbidden
  | LoginError { status :: Maybe StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: String -> String -> Aff LoginResponse
login user pass = do
  log "Sending login request to /api/login ..."
  result <- post RF.ignore "/api/login" (Just <<< RB.Json <<< encodeJson $ { username: user, password: pass })
  case result of
    Left err -> log ("Login failed: " <> printError err)
    Right r -> log ("Login status: " <> show r.status)
  pure
    $ case result of
        Left err -> LoginError { status: Nothing, message: printError err }
        Right r -> case r.status of
          StatusCode 403 -> LoginForbidden
          StatusCode 200 -> LoginOK
          other -> LoginError { status: Just other, message: r.statusText }

checkLogin :: Aff LoginResponse
checkLogin = do
  result <- get RF.ignore "/api/login/check"
  case result of
    Left err -> do
      pure $ LoginError { status: Nothing, message: printError err }
    Right r -> do
      pure
        $ case r.status of
            StatusCode 200 -> LoginOK
            StatusCode _ -> LoginForbidden

logout :: Aff Unit
logout = void $ get RF.ignore "/api/logout"

data RecoverBy
  = RecoverByEmail String
  | RecoverByZAddr String

type SignupRequest
  = { username :: String
    , password :: String
    , recoverBy :: RecoverBy
    , captchaToken :: String
    , invitation_codes :: Array String
    }

signupRequest :: String -> String -> RecoverBy -> String -> Array String -> SignupRequest
signupRequest username password recoverBy captchaToken invcodes = { username, password, recoverBy, captchaToken, invitation_codes: invcodes }

data SignupResponse
  = SignupOK
  | CaptchaInvalid
  | ZAddrInvalid
  | UsernameTaken
  | ServiceError (Maybe StatusCode) String

instance srShow :: Show SignupResponse where
  show r = case r of
    SignupOK -> "SignupOK"
    CaptchaInvalid -> "CaptchaInvalid"
    ZAddrInvalid -> "ZAddrInvalid"
    UsernameTaken -> "UsernameTaken"
    ServiceError _ _ -> "ServiceError"

data UsernameCheckResponse
  = UsernameCheckOK
  | UsernameCheckTaken

data ZAddrCheckResponse
  = ZAddrCheckValid
  | ZAddrCheckInvalid

checkUsername :: String -> Aff UsernameCheckResponse
checkUsername uname = do
  result <- get RF.ignore ("/api/validate_username?username=" <> uname)
  pure
    $ case result of
        Left err -> UsernameCheckTaken
        Right r
          | r.status == StatusCode 200 -> UsernameCheckOK
        Right r -> UsernameCheckTaken

checkZAddr :: String -> Aff ZAddrCheckResponse
checkZAddr zaddr = do
  result <- get RF.ignore ("/api/validate_zaddr?zaddr=" <> zaddr)
  pure
    $ case result of
        Left err -> ZAddrCheckInvalid
        Right r
          | r.status == StatusCode 200 -> ZAddrCheckValid
        Right r -> ZAddrCheckInvalid

signup :: SignupRequest -> Aff SignupResponse
signup req = do
  let
    signupJSON =
      encodeJson
        $ { username: req.username
          , password: req.password
          , recoveryType:
              case req.recoverBy of
                RecoverByEmail _ -> "email"
                RecoverByZAddr _ -> "zaddr"
          , recoveryEmail:
              case req.recoverBy of
                RecoverByEmail email -> Just email
                RecoverByZAddr _ -> Nothing
          , recoveryZAddr:
              case req.recoverBy of
                RecoverByEmail _ -> Nothing
                RecoverByZAddr zaddr -> Just zaddr
          , captchaToken: req.captchaToken
          }
  log ("Sending JSON request: " <> stringify signupJSON)
  result <- post RF.ignore "/api/register" (Just <<< RB.Json $ signupJSON)
  case result of
    Left err -> do
      log ("Registration failed: " <> printError err)
      pure (ServiceError Nothing $ printError err)
    Right r
      | r.status == StatusCode 200 -> do
        log "Registration succeeded!"
        pure SignupOK
    Right r
      | r.status == StatusCode 403 -> do
        log ("Registration failed: Capcha Invalid")
        pure CaptchaInvalid
    Right r
      | r.status == StatusCode 400 -> do
        log ("Registration failed: Z-Address Invalid")
        pure ZAddrInvalid
    Right r -> do
      log ("Registration failed: " <> r.statusText)
      pure $ ServiceError (Just r.status) r.statusText
