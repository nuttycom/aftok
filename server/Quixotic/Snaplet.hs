{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Quixotic.Snaplet where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Database.PostgreSQL.Simple

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

snapError :: MonadSnap m => Int -> Text -> m a
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  getResponse >>= finishWith

ok :: MonadSnap m => m a
ok = do
  modifyResponse $ setResponseCode 200
  getResponse >>= finishWith 


