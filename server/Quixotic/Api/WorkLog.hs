{-# LANGUAGE NoImplicitPrelude #-}

module Api.Worklog (resource) where

import ClassyPrelude

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (liftIO)
import Data.Set (Set)
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import Rest (Handler, ListHandler, Range (count, offset), Resource, Void, domainReason, mkInputHandler, mkListing, mkResourceReader, named, singleRead,
             withListing, xmlJsonE, xmlJsonI, xmlJsonO)
import qualified Rest.Resource as R

import ApiTypes (BlogApi, ServerData (..))
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo

resource :: 
