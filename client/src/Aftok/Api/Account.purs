module Aftok.Api.Account where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Affjax (post, get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Web.Event.Event as WE
import Halogen.HTML.Properties as P

import CSS (backgroundImage, url)

import Landkit.Card as Card

import Aftok.Types (System)

type LoginRequest = { username :: String, password :: String }

data LoginResponse
  = LoginOK
  | LoginForbidden
  | LoginError { status :: Maybe StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: String -> String -> Aff LoginResponse
login user pass = do
  log "Sending login request to /api/login ..."
  result <- post RF.ignore "/api/login" (Just <<< RB.Json <<< encodeJson $ { username: user, password : pass })
  case result of
       Left err -> log ("Login failed: " <> printError err)
       Right r  -> log ("Login status: " <> show r.status)
  pure $ case result of
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
      pure $ case r.status of
        StatusCode 200 -> LoginOK
        StatusCode _   -> LoginForbidden

logout :: Aff Unit
logout = void $ get RF.ignore "/api/logout"

data RecoverBy
  = RecoverByEmail String
  | RecoverByZAddr String 

type SignupRequest =
  { username :: String
  , password :: String
  , recoverBy :: RecoverBy
  , captchaToken :: String
  }

signupRequest :: String -> String -> RecoverBy -> String -> SignupRequest
signupRequest username password recoverBy captchaToken = 
  { username, password, recoverBy, captchaToken }

data SignupResponse
  = SignupOK
  | CaptchaInvalid
  | ZAddrInvalid
  | UsernameTaken

data UsernameCheckResponse
  = UsernameCheckOK
  | UsernameCheckTaken

data ZAddrCheckResponse
  = ZAddrCheckOK
  | ZAddrCheckInvalid

checkUsername :: String -> Aff UsernameCheckResponse
checkUsername uname = do
  pure UsernameCheckOK

checkZAddr :: String -> Aff ZAddrCheckResponse
checkZAddr zaddr = do
  pure ZAddrCheckOK

signup :: SignupRequest -> Aff SignupResponse
signup req = do
  let signupJSON = encodeJson $ 
        { username: req.username
        , password: req.password
        , recoveryType: case req.recoverBy of
                             RecoverByEmail _ => "email"
                             RecoverByZAddr _ => "zaddr"
        , email: case req.recoverBy of
                      RecoverByEmail email -> Just email
                      RecoverByZAddr _ -> Nothing
        , zaddr: case req.recoverBy of
                      RecoverByEmail _ -> Nothing
                      RecoverByZAddr zaddr -> Just zaddr
        , captchaToken: req.captchaToken
        }
  result <- post RF.ignore "/api/register" (Just <<< RB.Json $ signupJson)
  case result of
       Left err -> log ("Registration failed: " <> printError err)
       Right r  -> log ("Registration status: " <> show r.status)

  
