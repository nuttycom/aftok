{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.Server.Orphans where

import Aftok.Types (ProjectId (..))
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Servant.API (FromHttpApiData (..))

instance MonadIO m => MonadRandom (ReaderT r m) where
  getRandomBytes = liftIO . getRandomBytes

instance FromHttpApiData ProjectId where
  parseUrlPiece = fmap ProjectId . parseUrlPiece
