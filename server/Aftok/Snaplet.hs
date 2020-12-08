{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet where

import Aftok.Auction (AuctionId (..))
import Aftok.Currency.Bitcoin (NetworkMode (..))
import Aftok.Database
  ( DBError (..),
    DBOp,
    liftdb,
  )
import Aftok.Database.PostgreSQL (runQDBM)
import Aftok.Types
  ( ProjectId (..),
    UserId (..),
  )
import Aftok.Util
import Control.Lens
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString
  ( Parser,
    parseOnly,
    takeByteString,
  )
import Data.UUID (UUID, fromASCIIBytes)
import Snap.Core
  ( MonadSnap,
    finishWith,
    getParam,
    getResponse,
    logError,
    modifyResponse,
    readRequestBody,
    setResponseCode,
    setResponseStatus,
    writeLBS,
    writeText,
  )
import Snap.Snaplet as S
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.PostgresqlSimple
  ( HasPostgres (..),
    Postgres,
    liftPG,
    setLocalPostgresState,
  )
import Snap.Snaplet.Session (SessionManager)

data App
  = App
      { _networkMode :: NetworkMode,
        _sess :: Snaplet SessionManager,
        _db :: Snaplet Postgres,
        _auth :: Snaplet (AU.AuthManager App)
      }

makeLenses ''App

instance HasPostgres (S.Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

class HasNetworkMode m where
  getNetworkMode :: m NetworkMode

instance HasNetworkMode (S.Handler b App) where
  getNetworkMode = _networkMode <$> get

snapEval ::
  (MonadSnap m, HasPostgres m, HasNetworkMode m) => Program DBOp a -> m a
snapEval p = do
  let handleDBError (OpForbidden (UserId uid) reason) =
        snapError 403 $ show reason <> " (User " <> show uid <> ")"
      handleDBError (SubjectNotFound) =
        snapError
          404
          "The subject of the requested operation could not be found."
      handleDBError (EventStorageFailed) =
        snapError 500 "The event submitted could not be saved to the log."
  nmode <- getNetworkMode
  e <- liftPG $
    \conn -> liftIO $ runExceptT (runQDBM nmode conn $ interpret liftdb p)
  either handleDBError pure e

snapError :: MonadSnap m => Int -> Text -> m a
snapError c t = do
  let errBytes = encodeUtf8 t
  logError errBytes
  modifyResponse $ setResponseStatus c errBytes
  writeText (show c <> " - " <> t)
  getResponse >>= finishWith

snapErrorJS :: (A.ToJSON err, MonadSnap m) => Int -> Text -> err -> m a
snapErrorJS c t err = do
  let errBytes = A.encode err
  logError (fromLazy errBytes)
  modifyResponse $ setResponseStatus c (encodeUtf8 t)
  writeLBS errBytes
  getResponse >>= finishWith

ok :: MonadSnap m => m a
ok = do
  modifyResponse $ setResponseCode 200
  getResponse >>= finishWith

requireParam :: MonadSnap m => Text -> m ByteString
requireParam name = do
  maybeBytes <- getParam (encodeUtf8 name)
  maybe
    (snapError 400 $ "Parameter " <> show name <> " is required")
    pure
    maybeBytes

parseParam ::
  MonadSnap m =>
  -- | the name of the parameter to be parsed
  Text ->
  -- | parser for the value of the parameter
  Parser a ->
  -- | the parsed value
  m a
parseParam name parser = do
  bytes <- requireParam name
  either
    ( const
        . snapError 400
        $ "Value of parameter "
          <> show name
          <> " could not be parsed to a valid value."
    )
    pure
    (parseOnly parser bytes)

requireId ::
  MonadSnap m =>
  -- | name of the parameter
  Text ->
  -- | constructor for the identifier
  (UUID -> a) ->
  m a
requireId name f = do
  maybeId <- parseParam name idParser
  maybe
    ( snapError 400 $ "Value of parameter \"" <> name <> "\" is not a valid UUID"
    )
    pure
    maybeId
  where
    idParser = do
      bs <- takeByteString
      pure $ f <$> fromASCIIBytes bs

readRequestJSON :: MonadSnap m => Word64 -> m A.Value
readRequestJSON len = do
  requestBody <- A.decode <$> readRequestBody len
  maybe
    (snapError 400 "Could not interpret request body as a nonempty JSON value.")
    pure
    requestBody

requireProjectId :: MonadSnap m => m ProjectId
requireProjectId = requireId "projectId" ProjectId

requireAuctionId :: MonadSnap m => m AuctionId
requireAuctionId = requireId "auctionId" AuctionId
