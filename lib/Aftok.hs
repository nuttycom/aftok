{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Aftok where

import           ClassyPrelude

import           Control.Lens     (makeLenses, makePrisms)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Data.UUID

import           Network.Haskoin.Crypto (Address(..), base58ToAddr) 

newtype BtcAddr = BtcAddr Address deriving (Show, Eq, Ord)
makePrisms ''BtcAddr

parseBtcAddr :: Text -> Maybe BtcAddr
parseBtcAddr addr = BtcAddr <$> (base58ToAddr . encodeUtf8) addr

instance FromJSON BtcAddr where
  parseJSON v = do
    t <- parseJSON v
    maybe (fail $ show t <> " is not a valid BTC address") pure $ parseBtcAddr t

newtype Months = Months Integer
  deriving (Eq, Show, Data, Typeable)

data DepreciationFunction = LinearDepreciation Months Months
  deriving (Eq, Show, Data, Typeable)

newtype UserId = UserId UUID deriving (Show, Eq, Ord)
makePrisms ''UserId

newtype UserName = UserName Text deriving (Show, Eq)
makePrisms ''UserName

newtype Email = Email Text deriving (Show, Eq)
makePrisms ''Email

data User = User
  { _username    :: UserName
  , _userAddress :: Maybe BtcAddr
  , _userEmail   :: Email
  }
makeLenses ''User

--                        | others tbd

instance ToJSON DepreciationFunction where
  toJSON (LinearDepreciation (Months up) (Months dp)) =
    object [ "type" .= ("LinearDepreciation" :: Text)
           , "arguments" .= object [ "undep" .= up
                                   , "dep" .= dp
                                   ]
           ]

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

