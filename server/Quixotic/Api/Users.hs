{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Api.Users 
  ( loginHandler
  , registerHandler
  ) where

import ClassyPrelude 

import Data.ByteString (split)
import Data.Attoparsec.ByteString 
import qualified Data.ByteString.Base64 as B64

import Quixotic.Api.Types

import Snap.Core
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU

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

-- data CreateUser = CreateUser
--   { _user :: User
--   , _password :: ByteString
--   }
-- makeLenses ''CreateUser

loginHandler :: (AU.AuthUser -> Handler App App a) -> Handler App App a
loginHandler onSuccess = do
  req <- getRequest
  rawHeader <- maybe throwChallenge pure $ getHeader "Authorization" req
  (AuthHeader uname pwd) <- either (throwDenied . AU.AuthError) pure $ parseOnly authHeaderParser rawHeader 
  authResult <- with auth $ AU.loginByUsername uname (AU.ClearText pwd) False
  either throwDenied onSuccess authResult

registerHandler :: Handler App App ()
registerHandler = ok

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
