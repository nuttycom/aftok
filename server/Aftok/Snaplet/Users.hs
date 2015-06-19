{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Users 
  ( registerHandler
  ) where

import ClassyPrelude 

import Control.Lens
import Data.Aeson as A
import Data.Thyme.Clock as C

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
  , _invitationCodes :: [InvitationCode]
  }
makeLenses ''CUser

instance FromJSON CUser where
  parseJSON (Object v) = 
    let parseUser = User <$> (UserName <$> v .: "username")
                         <*> (BtcAddr  <$> v .: "btcAddr")
                         <*> (Email    <$> v .: "email")

        parseInvitationCodes c = either 
          (\e -> fail $ "Invitation code was rejected as invalid: " <> e) 
          pure 
          (traverse parseInvCode c)

    in  CU <$> parseUser
           <*> (fromString <$> v .: "password")
           <*> (parseInvitationCodes =<< v .: "invitation_codes")

  parseJSON _ = mzero

registerHandler :: Handler App App UserId
registerHandler = do
  requestBody <- readRequestBody 4096
  -- allow any number of 'invitationCode' query parameters
  userData <- maybe (snapError 400 "Could not parse user data") pure $ A.decode requestBody
  t <- liftIO C.getCurrentTime
  let createSUser = AU.createUser (userData ^. (cuser.username._UserName)) (userData ^. password)
      createQUser = snapEval $ do
        userId <- createUser $ userData ^. cuser
        void $ traverse (\c -> acceptInvitation userId c t) (userData ^. invitationCodes)
        return userId
  authUser <- with auth createSUser
  either throwDenied (\_ -> createQUser) authUser 
