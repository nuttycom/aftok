{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveDataTypeable #-}

module Aftok where

import ClassyPrelude

import Control.Lens(makePrisms, makeLenses)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Base64.URL as B64
import Data.Data
import Data.Thyme.Clock as C
import Data.UUID
import OpenSSL.Random

newtype BtcAddr = BtcAddr Text deriving (Show, Eq, Ord)
makePrisms ''BtcAddr

parseBtcAddr :: Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- FIXME: perform validation

newtype Months = Months Integer 
  deriving (Eq, Show, Data, Typeable)

data DepreciationFunction = LinearDepreciation Months Months
  deriving (Eq, Show, Data, Typeable)

newtype UserId = UserId UUID deriving (Show, Eq)
makePrisms ''UserId

newtype UserName = UserName Text deriving (Show, Eq)
makePrisms ''UserName

newtype Email = Email Text deriving (Show, Eq)
makePrisms ''Email

data User = User
  { _username :: UserName
  , _userAddress :: BtcAddr
  , _userEmail :: Email
  }
makeLenses ''User

newtype ProjectId = ProjectId UUID deriving (Show, Eq)
makePrisms ''ProjectId

type ProjectName = Text
data Project = Project
  { _projectName :: ProjectName
  , _inceptionDate :: C.UTCTime
  , _initiator :: UserId
  , _depf :: DepreciationFunction
  }
makeLenses ''Project

newtype InvitationCode = InvitationCode ByteString deriving (Eq)
makePrisms ''InvitationCode

randomInvCode :: IO InvitationCode
randomInvCode = InvitationCode <$> randBytes 256

parseInvCode :: Text -> Either String InvitationCode
parseInvCode t = do
  code <- B64.decode . encodeUtf8 $ t
  if length code == 256
    then Right $ InvitationCode code
    else Left "Invitation code appears to be invalid."

renderInvCode :: InvitationCode -> Text
renderInvCode (InvitationCode bs) = decodeUtf8 $ B64.encode bs

data Invitation = Invitation 
  { _projectId :: ProjectId
  , _invitingUser :: UserId
  , _invitedEmail :: Email
  , _invitationTime :: C.UTCTime
  , _acceptanceTime :: Maybe C.UTCTime
  }
makeLenses ''Invitation

--                        | others tbd

instance ToJSON DepreciationFunction where
  toJSON (LinearDepreciation (Months up) (Months dp)) =
    object [ "type" .= ("LinearDepreciation" :: Text)
           , "arguments" .= (
             object [ "undep" .= up
                    , "dep" .= dp
                    ]
           )]

instance FromJSON DepreciationFunction where
  parseJSON (Object v) = do
    t <- v .: "type" :: Parser Text
    args <- v .: "arguments"
    case unpack t of
      "LinearDepreciation" -> 
        let undep = Months <$> (args .: "undep")
            dep   = Months <$> (args .: "dep")
        in  LinearDepreciation <$> undep <*> dep
      x -> fail $ "No depreciation function recognized for type " <> x

  parseJSON _ = mzero

