{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Aftok.Snaplet where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString(Parser, parseOnly)

import Aftok
import Aftok.Database
import Aftok.Database.PostgreSQL
import Aftok.Util

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.Session

data App = App 
  { _sess :: Snaplet SessionManager
  , _db   :: Snaplet Postgres
  , _auth :: Snaplet (AU.AuthManager App)
  }
makeLenses ''App

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

snapEval :: (MonadSnap m, HasPostgres m) => DBProg a -> m a
snapEval p = do
  let handleDBError (OpForbidden (UserId uid) reason) = 
        snapError 403 $ tshow reason <> " (User " <> tshow uid <> ")"
      handleDBError (SubjectNotFound) = 
        snapError 404 "The subject of the requested operation could not be found."

  e <- liftPG $ \conn -> runEitherT (runQDBM conn $ interpret dbEval p)
  either handleDBError pure e

snapError :: MonadSnap m => Int -> Text -> m a
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  getResponse >>= finishWith

ok :: MonadSnap m => m a
ok = do
  modifyResponse $ setResponseCode 200
  getResponse >>= finishWith 

parseParam :: MonadSnap m => ByteString -> Parser a -> m a
parseParam name parser = do
  maybeBytes <- getParam name
  case maybeBytes of
    Nothing -> snapError 400 $ "Parameter "<> tshow name <>" is required"
    Just bytes -> either
      (const . snapError 400 $ "Value of parameter "<> tshow name <>" could not be parsed to a valid value.")
      pure
      (parseOnly parser bytes)

readRequestJSON :: MonadSnap m => Int64 -> m A.Value
readRequestJSON i = do
  requestBody <- A.decode <$> readRequestBody i
  maybe (snapError 400 "Could not interpret request body as a nonempty JSON value.") pure requestBody


