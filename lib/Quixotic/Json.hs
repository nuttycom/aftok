{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Quixotic.Json where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson 
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as C
import Data.Data
import Data.List.NonEmpty as L
import Data.Map.Strict as MS

import Quixotic
import Quixotic.Database
import Quixotic.Interval
import Quixotic.TimeLog

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

data Version = Version { majorVersion :: Word8
                       , minorVersion :: Word8
                       , trivialVersion :: Word8 
                       } deriving (Typeable, Data)

printVersion :: Version -> Text
printVersion Version{..} = intercalate "." (fmap tshow [majorVersion, minorVersion, trivialVersion])

versionParser :: P.Parser Version
versionParser = Version <$> P.decimal <*> (P.char '.' >> P.decimal) <*> (P.char '.' >> P.decimal)

versioned :: Version -> Value -> Value
versioned ver v = object [ "schemaVersion" .= printVersion ver
                         , "value" .= v ]

version :: QuasiQuoter 
version = QuasiQuoter { quoteExp = quoteVersionExp
                      , quotePat = error "Pattern quasiquotation of versions not supported."
                      , quoteType = error "Type quasiquotation of versions not supported."
                      , quoteDec = error "Dec quasiquotation of versions not supported."
                      }


quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  v <- either (fail . show) pure $ P.parseOnly versionParser (C.pack s)
  dataToExpQ (const Nothing) v

unversion :: (Version -> Value -> Parser a) -> Value -> Parser a
unversion f (Object v) = do
  vers <- v .: "schemaVersion"
  vers' <- either (\_ -> mzero) pure $ P.parseOnly versionParser (encodeUtf8 vers)
  value <- v .: "value"
  f vers' value
unversion _ _ = mzero

qdbProjectJSON :: QDBProject -> Value
qdbProjectJSON qp = 
  object [ "projectId" .= (qp ^. (projectId._ProjectId))
         , "project" .= projectJSON (qp ^. project)
         ]

projectJSON :: Project -> Value
projectJSON p = 
  object [ "projectName"    .= (p ^. projectName)
         , "inceptionDate"  .= (p ^. inceptionDate)
         , "initiator"      .= (p ^. (initiator._UserId)) ]

payoutsJSON :: Payouts -> Value
payoutsJSON (Payouts m) = toJSON $ MS.mapKeys (^. _BtcAddr) m

parsePayoutsJSON :: Value -> Parser Payouts
parsePayoutsJSON v = 
  Payouts . MS.mapKeys BtcAddr <$> parseJSON v 

workIndexJSON :: WorkIndex -> Value
workIndexJSON (WorkIndex widx) = 
  toJSON $ (L.toList . fmap intervalJSON) <$> (MS.mapKeysMonotonic (^._BtcAddr) widx)

eventIdJSON :: EventId -> Value
eventIdJSON (EventId eid) = toJSON eid

