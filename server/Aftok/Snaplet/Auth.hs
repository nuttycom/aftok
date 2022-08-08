{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.Auth where

import Aftok.Database
import Aftok.Snaplet
import Aftok.Types
import Aftok.Util.Http (authHeaderParser)
import Control.Error.Util (maybeT)
import Control.Lens
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Attoparsec.ByteString (parseOnly)
import Snap.Core
import Snap.Snaplet as S
import qualified Snap.Snaplet.Auth as AU

data LoginRequest = LoginRequest
  { loginUser :: Text,
    loginPass :: Text
  }

parseLoginRequest :: A.Value -> A.Parser LoginRequest
parseLoginRequest (A.Object o) =
  LoginRequest <$> o .: "username" <*> o .: "password"
parseLoginRequest val = fail $ "Value " <> show val <> " is not a JSON object."

requireLogin :: S.Handler App App AU.AuthUser
requireLogin = do
  requireLoginWith (const throwChallenge)

requireLoginWith ::
  (forall a. () -> S.Handler App App a) -> S.Handler App App AU.AuthUser
requireLoginWith throwMissingAuth = do
  req <- getRequest
  rawHeader <- maybe (throwMissingAuth ()) pure $ getHeader "Authorization" req
  (uname, pwd) <-
    either (throwDenied . AU.AuthError) pure $
      parseOnly authHeaderParser rawHeader
  authResult <- with auth $ AU.loginByUsername uname (AU.ClearText pwd) False
  either throwDenied pure authResult

requireLoginXHR :: S.Handler App App AU.AuthUser
requireLoginXHR = do
  requestBody <- readRequestBody 4096
  credentials <-
    case A.eitherDecode requestBody >>= A.parseEither parseLoginRequest of
      Left _ -> snapError 400 $ "Unable to parse login credentials object."
      Right creds -> pure creds
  authResult <-
    with auth $
      AU.loginByUsername
        (loginUser credentials)
        (AU.ClearText (encodeUtf8 $ loginPass credentials))
        False
  either throwDenied pure authResult

requireUser :: S.Handler App App AU.AuthUser
requireUser = do
  currentUser <- with auth AU.currentUser
  maybe
    (requireLoginWith $ const (throwDenied $ AU.AuthError "Not Authenticated"))
    pure
    currentUser

requireUserId :: S.Handler App App UserId
requireUserId = do
  currentUser <- UserName . AU.userLogin <$> requireUser
  maybeT
    (snapError 500 "Unable to retrieve user record for authenticated user")
    (pure . (^. _1))
    (mapMaybeT snapEval $ findUserByName currentUser)

throwChallenge :: MonadSnap m => m a
throwChallenge = do
  modifyResponse $
    (setResponseStatus 401 "Unauthorized")
      . (setHeader "WWW-Authenticate" "Basic realm=aftok")
  getResponse >>= finishWith

throwDenied :: MonadSnap m => AU.AuthFailure -> m a
throwDenied failure = do
  modifyResponse $ setResponseStatus 403 "Access Denied"
  logError (encodeUtf8 $ "Access Denied: " <> show @Text failure)
  getResponse >>= finishWith
