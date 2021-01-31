{-# LANGUAGE TemplateHaskell #-}

module Aftok.Project where

import Aftok.Types
import Control.Lens
  ( makeLenses,
    makePrisms,
  )
import Crypto.Random.Types
  ( MonadRandom,
    getRandomBytes,
  )
import qualified Data.ByteString as BS
import Data.ByteString.Base64.URL as B64
import Data.Thyme.Clock as C

type ProjectName = Text

data Project
  = Project
      { _projectName :: ProjectName,
        _inceptionDate :: C.UTCTime,
        _initiator :: UserId,
        _depRules :: DepreciationRules
      }

makeLenses ''Project

newtype InvitationCode = InvitationCode ByteString deriving (Eq)

makePrisms ''InvitationCode

randomInvCode :: (MonadRandom m) => m InvitationCode
randomInvCode = InvitationCode <$> getRandomBytes 32

parseInvCode :: Text -> Either Text InvitationCode
parseInvCode t = do
  code <- B64.decodeBase64 . encodeUtf8 $ t
  if BS.length code == 32
    then Right $ InvitationCode code
    else Left "Invitation code appears to be invalid."

renderInvCode :: InvitationCode -> Text
renderInvCode (InvitationCode bs) = B64.encodeBase64 bs

data Invitation
  = Invitation
      { _projectId :: ProjectId,
        _invitingUser :: UserId,
        _invitedEmail :: Email,
        _invitationTime :: C.UTCTime,
        _acceptanceTime :: Maybe C.UTCTime
      }

makeLenses ''Invitation
