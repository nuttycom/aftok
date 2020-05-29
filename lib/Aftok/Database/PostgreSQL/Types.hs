module Aftok.Database.PostgreSQL.Types where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Aftok.TimeLog.Serialization    ( depfFromJSON
                                                , depfToJSON
                                                )
import           Aftok.Types                    ( DepreciationFunction )

newtype SerDepFunction = SerDepFunction { unSerDepFunction :: DepreciationFunction }

instance FromJSON SerDepFunction where
  parseJSON v = SerDepFunction <$> depfFromJSON v

instance ToJSON SerDepFunction where
  toJSON (SerDepFunction depf) = depfToJSON depf
