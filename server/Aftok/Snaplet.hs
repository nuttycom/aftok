{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Aftok.Snaplet where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as A
import Database.PostgreSQL.Simple

import Aftok.Database
import Aftok.Database.PostgreSQL
import Aftok.TimeLog

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

readRequestJSON :: MonadSnap m => Int64 -> m A.Value
readRequestJSON i = do
  requestBody <- A.decode <$> readRequestBody i
  maybe (snapError 400 "Could not interpret request body as a nonempty JSON value.") pure requestBody

