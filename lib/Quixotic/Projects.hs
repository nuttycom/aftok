{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Projects where

import ClassyPrelude
import Control.Lens

import Network.Bitcoin

import Quixotic
import Quixotic.Users


newtype ProjectId = ProjectId Int64 deriving (Show, Eq)
makePrisms ''ProjectId

data Project = Project
  { _projectName :: Text
  , _inceptionDate :: UTCTime
  , _initiator :: UserId
  }
makeLenses ''Project

data Invitation = Invitation
  { _projectId :: ProjectId
  , _currentMember :: UserId
  , _sentAt :: UTCTime
  , _toAddr   :: BtcAddr
  , _amount :: BTC
  }
makeLenses ''Invitation

data Acceptance = Acceptance
  { _acceptedInvitation :: Int64
  , _blockHeight :: Integer
  , _observedAt :: UTCTime
  }
makeLenses ''Acceptance

data Cancellation = Cancellation
  { _cancelledInvitation :: Int64
  , _requestedAt :: UTCTime
  }
makeLenses ''Cancellation
