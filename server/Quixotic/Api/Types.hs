{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Api.Types where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Database.PostgreSQL.Simple

import Quixotic
import Quixotic.Database
import Quixotic.Database.PostgreSQL
import Quixotic.TimeLog

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.Session

data QModules = QModules 
  { _qdb :: QDB (ReaderT Connection IO) 
  , _depf :: DepF
  }
makeLenses ''QModules

data App = App 
  { _qm  :: Snaplet QModules
  , _sess :: Snaplet SessionManager
  , _db   :: Snaplet Postgres
  , _auth :: Snaplet (AU.AuthManager App)
  }
makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

-- | FIXME, make configurable
qdbpgSnapletInit :: SnapletInit a QModules
qdbpgSnapletInit = makeSnaplet "qdbpg" "QDB on Postgresql" Nothing $ do
  pure $ QModules postgresQDB $ linearDepreciation (Months 6) (Months 60) 

requireLogin :: Handler App App a -> Handler App App a
requireLogin = AU.requireUser auth (redirect "/login")

requireUserId :: (UserId -> Handler App App a) -> Handler App App a 
requireUserId hf = AU.requireUser auth (redirect "/login") $ do
  QDB{..} <- view qdb <$> with qm get
  authedUser <- with auth AU.currentUser
  qdbUser <- case UserName . AU.unUid <$> (AU.userId =<< authedUser) of 
    Nothing -> snapError 403 "User is authenticated, but session lacks user identifier"
    Just n  -> liftPG . runReaderT $ findUserByUserName n
  case qdbUser of
    Nothing -> snapError 403 "Unable to retrieve user record for authenticated user" 
    Just u -> hf (u ^. userId)

checkProjectAccess :: ProjectId -> UserId -> Handler App App a
checkProjectAccess = undefined

snapError :: MonadSnap m => Int -> Text -> m a
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  getResponse >>= finishWith

ok :: MonadSnap m => m ()
ok = do
  modifyResponse $ setResponseCode 200
  getResponse >>= finishWith 
