{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Quixotic.Api.Users 
  ( loginHandler
  , registerHandler
  ) where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import Data.Aeson as A
import Data.ByteString (split)
import Data.Attoparsec.ByteString 
import qualified Data.ByteString.Base64 as B64

import Quixotic
import Quixotic.Database
import Quixotic.Users
import Quixotic.Api.Types

import Snap.Core
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.PostgresqlSimple

data AuthHeader = AuthHeader Text ByteString

authHeaderParser :: Parser AuthHeader
authHeaderParser = do
  let isBase64Char w = (w >= 47 && w <= 57 ) ||
                       (w >= 64 && w <= 90 ) ||
                       (w >= 97 && w <= 122) ||
                       (w == 43 || w == 61 )
  b64     <- string "Basic" *> takeWhile1 isBase64Char 
  decoded <- either fail pure $ B64.decode b64
  case split 58 decoded of
    (uname : pwd : []) -> pure $ AuthHeader (decodeUtf8 uname) pwd
    _ -> fail "Could not unpack auth header into username and password components"

data CreateUser = CreateUser
  { _cuser :: User
  , _password :: ByteString
  }
makeLenses ''CreateUser

instance FromJSON CreateUser where
  parseJSON (Object v) = 
    let u = User <$> (UserName <$> v .: "username")
                 <*> (BtcAddr  <$> v .: "btcAddr")
                 <*> v .: "email"
    in  CreateUser <$> u <*> (fromString <$> v .: "password")
  parseJSON _ = mzero

loginHandler :: (AU.AuthUser -> Handler App App a) -> Handler App App a
loginHandler onSuccess = do
  req <- getRequest
  rawHeader <- maybe throwChallenge pure $ getHeader "Authorization" req
  (AuthHeader uname pwd) <- either (throwDenied . AU.AuthError) pure $ parseOnly authHeaderParser rawHeader 
  authResult <- with auth $ AU.loginByUsername uname (AU.ClearText pwd) False
  either throwDenied onSuccess authResult

registerHandler :: Handler App App ()
registerHandler = do
  QDB{..} <- view qdb <$> with qm get
  requestBody <- readRequestBody 0
  userData <- maybe (snapError 400 "Could not parse user data") pure $ A.decode requestBody
  authUser <- with auth $ 
    AU.createUser (userData ^. (cuser.username._UserName)) (userData ^. password)
  let createQUser = liftPG $ runReaderT (createUser $ userData ^. cuser)
  void $ either throwDenied (\_ -> createQUser) authUser 

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

