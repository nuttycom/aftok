module Quixotic.Snaplet.Auth where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import Data.ByteString (split)
import Data.Attoparsec.ByteString 
import qualified Data.ByteString.Base64 as B64

import Quixotic
import Quixotic.Database
import Quixotic.Snaplet

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import qualified Snap.Snaplet.Auth as AU

type AuthHeader = (Text, ByteString)

authHeaderParser :: Parser AuthHeader
authHeaderParser = do
  let isBase64Char w = (w >= 47 && w <= 57 ) ||
                       (w >= 64 && w <= 90 ) ||
                       (w >= 97 && w <= 122) ||
                       (w == 43 || w == 61 )
  b64     <- string "Basic" *> takeWhile1 isBase64Char 
  decoded <- either fail pure $ B64.decode b64
  case split 58 decoded of
    (uname : pwd : []) -> pure $ (decodeUtf8 uname, pwd)
    _ -> fail "Could not unpack auth header into username and password components"

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
  QDB{..} <- view qdb <$> with qm get
  currentUser <- requireLogin
  qdbUser <- case UserName . AU.unUid <$> AU.userId currentUser of 
    Nothing -> snapError 403 "User is authenticated, but session lacks user identifier"
    Just n  -> liftPG . runReaderT $ findUserByUserName n
  case qdbUser of
    Nothing -> snapError 403 "Unable to retrieve user record for authenticated user" 
    Just u -> pure (u ^. userId)

requireProjectAccess :: UserId -> Handler App App ProjectId
requireProjectAccess uid = do
  pidMay <- getParam "projectId"
  case ProjectId <$> (readMay =<< fmap decodeUtf8 pidMay) of
    Nothing  -> snapError 403 "Value of parameter projectId could not be parsed to a valid value."
    Just pid -> error $ "FIXME: implement project access check - got pid " ++ " " ++ show uid ++ " " ++ show pid

throwChallenge :: MonadSnap m => m a
throwChallenge = do
    modifyResponse $ (setResponseStatus 401 "Unauthorized") . 
                     (setHeader "WWW-Authenticate" "Basic realm=quixotic")
    getResponse >>= finishWith

throwDenied :: MonadSnap m => AU.AuthFailure -> m a
throwDenied failure = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeText $ "Access Denied: " <> tshow failure
    getResponse >>= finishWith

