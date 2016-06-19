{-# LANGUAGE TemplateHaskell    #-}

module Aftok.Project where

import           ClassyPrelude

import           Control.Lens               (makeLenses, makePrisms)
import           Data.ByteString.Base64.URL as B64
import           Data.Thyme.Clock           as C
import           Data.UUID
import           OpenSSL.Random

import           Aftok

newtype ProjectId = ProjectId UUID deriving (Show, Eq, Ord)
makePrisms ''ProjectId

type ProjectName = Text
data Project = Project
  { _projectName   :: ProjectName
  , _inceptionDate :: C.UTCTime
  , _initiator     :: UserId
  , _depf          :: DepreciationFunction
  }
makeLenses ''Project

newtype InvitationCode = InvitationCode ByteString deriving (Eq)
makePrisms ''InvitationCode

randomInvCode :: IO InvitationCode
randomInvCode = InvitationCode <$> randBytes 32

parseInvCode :: Text -> Either String InvitationCode
parseInvCode t = do
  code <- B64.decode . encodeUtf8 $ t
  if length code == 32
    then Right $ InvitationCode code
    else Left "Invitation code appears to be invalid."

renderInvCode :: InvitationCode -> Text
renderInvCode (InvitationCode bs) = decodeUtf8 $ B64.encode bs

data Invitation = Invitation
  { _projectId      :: ProjectId
  , _invitingUser   :: UserId
  , _invitedEmail   :: Email
  , _invitationTime :: C.UTCTime
  , _acceptanceTime :: Maybe C.UTCTime
  }
makeLenses ''Invitation

