{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Snaplet.Users 
  ( registerHandler
  ) where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import Data.Aeson as A
import Quixotic
import Quixotic.Database
import Quixotic.Snaplet
import Quixotic.Snaplet.Auth

import Snap.Core
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.PostgresqlSimple

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

registerHandler :: Handler App App ()
registerHandler = do
  QDB{..} <- view qdb <$> with qm get
  requestBody <- readRequestBody 0
  userData <- maybe (snapError 400 "Could not parse user data") pure $ A.decode requestBody
  authUser <- with auth $ 
    AU.createUser (userData ^. (cuser.username._UserName)) (userData ^. password)
  let createQUser = liftPG $ runReaderT (createUser $ userData ^. cuser)
  void $ either throwDenied (\_ -> createQUser) authUser 
