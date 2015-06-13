module Aftok.Snaplet.Auth where

import ClassyPrelude 

import Control.Lens
-- import Control.Monad.State
import Data.Attoparsec.ByteString (parseOnly)

import Aftok
import Aftok.Database
import Aftok.Util.Http (authHeaderParser)
import Aftok.Snaplet

import Snap.Core
import Snap.Snaplet
-- import Snap.Snaplet.PostgresqlSimple
import qualified Snap.Snaplet.Auth as AU

requireLogin :: Handler App App AU.AuthUser 
requireLogin = do
  req <- getRequest
  rawHeader    <- maybe throwChallenge pure $ getHeader "Authorization" req
  (uname, pwd) <- either (throwDenied . AU.AuthError) pure $ parseOnly authHeaderParser rawHeader 
  authResult   <- with auth $ AU.loginByUsername uname (AU.ClearText pwd) False
  either throwDenied pure authResult

requireUser :: Handler App App AU.AuthUser 
requireUser = do 
  currentUser <- with auth AU.currentUser
  maybe requireLogin pure currentUser

requireUserId :: Handler App App UserId
requireUserId = do
  currentUser <- UserName . AU.userLogin <$> requireLogin
  qdbUser <- snapEval $ findUserByName currentUser
  case qdbUser of
    Nothing -> snapError 403 "Unable to retrieve user record for authenticated user" 
    Just u -> pure (u ^. _1)

requireProjectId :: Handler App App ProjectId
requireProjectId = do
  pidMay <- getParam "projectId"
  case ProjectId <$> (readMay =<< fmap decodeUtf8 pidMay) of
    Nothing  -> snapError 400 "Value of parameter projectId could not be parsed to a valid value."
    Just pid -> pure pid

throwChallenge :: MonadSnap m => m a
throwChallenge = do
    modifyResponse $ (setResponseStatus 401 "Unauthorized") . 
                     (setHeader "WWW-Authenticate" "Basic realm=aftok")
    getResponse >>= finishWith

throwDenied :: MonadSnap m => AU.AuthFailure -> m a
throwDenied failure = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeText $ "Access Denied: " <> tshow failure
    getResponse >>= finishWith

