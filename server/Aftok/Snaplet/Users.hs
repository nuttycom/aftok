{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Users 
  ( registerHandler
  ) where

import ClassyPrelude 

import Control.Lens
import Data.Aeson as A
import Aftok
import Aftok.Database
import Aftok.Snaplet
import Aftok.Snaplet.Auth

import Snap.Core
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU

data CUser = CU
  { _cuser :: User
  , _password :: ByteString
  }
makeLenses ''CUser

instance FromJSON CUser where
  parseJSON (Object v) = 
    let u = User <$> (UserName <$> v .: "username")
                 <*> (BtcAddr  <$> v .: "btcAddr")
                 <*> v .: "email"
    in  CU <$> u <*> (fromString <$> v .: "password")
  parseJSON _ = mzero

registerHandler :: Handler App App ()
registerHandler = do
  requestBody <- readRequestBody 4096
  userData <- maybe (snapError 400 "Could not parse user data") pure $ A.decode requestBody
  let createSUser = AU.createUser (userData ^. (cuser.username._UserName)) (userData ^. password)
      createQUser = snapEval (createUser $ userData ^. cuser)
  authUser <- with auth createSUser
  void $ either throwDenied (\_ -> createQUser) authUser 
