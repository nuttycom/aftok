{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Aftok.Json where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson 
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as C
import Data.Data
import Data.List.NonEmpty as L
import Data.Map.Strict as MS

import Aftok
import Aftok.Database
import Aftok.Interval
import Aftok.TimeLog

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

data Version = Version { majorVersion :: Word8
                       , minorVersion :: Word8
                       } deriving (Typeable, Data)

instance Show Version where
  show Version{..} = intercalate "." $ fmap show [majorVersion, minorVersion]

versionParser :: P.Parser Version
versionParser = Version <$> P.decimal <*> (P.char '.' >> P.decimal) 

version :: QuasiQuoter 
version = QuasiQuoter { quoteExp = quoteVersionExp
                      , quotePat = error "Pattern quasiquotation of versions not supported."
                      , quoteType = error "Type quasiquotation of versions not supported."
                      , quoteDec = error "Dec quasiquotation of versions not supported."
                      }

-- TODO: Include source location information, and implement quote patterns.
quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  v <- either (fail . show) pure $ P.parseOnly versionParser (C.pack s)
  dataToExpQ (const Nothing) v

versioned :: Version -> Value -> Value
versioned ver v = object [ "schemaVersion" .= tshow ver
                         , "value" .= v ]

{-| 
 - Convenience function to allow dispatch of different serialized
 - versions to different parsers.
 -}
unversion :: (Version -> Value -> Parser a) -> Value -> Parser a
unversion f (Object v) = do
  verstr   <- v .: "schemaVersion"
  vers  <- either fail pure $ P.parseOnly versionParser (encodeUtf8 verstr)
  v .: "value" >>= f vers

unversion _ x = 
  fail $ show x <> " did not contain the expected version information."

--------------
-- Versions --
--------------

v1 :: Value -> Value
v1 = versioned $ Version 1 0

unv1 :: String -> (Value -> Parser a) -> Value -> Parser a
unv1 name f v =
  let p (Version 1 0) = f 
      p ver = const . fail $ "Unrecognized " <> name <> " schema version: " <> show ver
  in  unversion p v

-----------------
-- Serializers --
-----------------

qdbProjectJSON :: KeyedProject -> Value
qdbProjectJSON (pid, project) = v1 $
  object [ "projectId" .=  (tshow $ pid ^. _ProjectId)
         , "project" .= projectJSON project
         ]

projectJSON :: Project -> Value
projectJSON p = v1 $
  object [ "projectName"    .= (p ^. projectName)
         , "inceptionDate"  .= (p ^. inceptionDate)
         , "initiator"      .= (tshow $ p ^. (initiator._UserId)) ]

payoutsJSON :: Payouts -> Value
payoutsJSON (Payouts m) = v1 $
  toJSON $ MS.mapKeys (^. _BtcAddr) m

workIndexJSON :: WorkIndex -> Value
workIndexJSON (WorkIndex widx) = v1 $
  toJSON $ (L.toList . fmap intervalJSON) <$> (MS.mapKeysMonotonic (^._BtcAddr) widx)

eventIdJSON :: EventId -> Value
eventIdJSON (EventId eid) = v1 $
  object [ "eventId" .= tshow eid ]

logEntryJSON :: LogEntry -> Value
logEntryJSON (LogEntry a ev m) = v1 $
  object [ "btcAddr"   .= (a ^. _BtcAddr)
         , "eventType" .= eventName ev
         , "eventTime" .= (ev ^. eventTime)
         , "eventMeta" .= m
         ]

amendmentIdJSON :: AmendmentId -> Value
amendmentIdJSON (AmendmentId aid) = v1 $
  object [ "amendmentId" .= tshow aid ]

-------------
-- Parsers --
-------------

parsePayoutsJSON :: Value -> Parser Payouts
parsePayoutsJSON = unv1 "payouts" $ \v ->
  Payouts . MS.mapKeys BtcAddr <$> parseJSON v 

parseEventAmendment :: ModTime -> Value -> Parser EventAmendment
parseEventAmendment t = 
  let parseA x "timeChange" = TimeChange t <$> x .: "eventTime"
      parseA x "addrChage"  = do
        addrText <- x .: "btcAddr" 
        maybe 
          (fail $ (show addrText) <> "is not a valid BTC address") 
          (pure . AddressChange t) 
          $ parseBtcAddr addrText
      parseA x "metadataChange" = 
        MetadataChange t <$> x .: "eventMeta"
      parseA _ other = 
        fail $ "Amendment value " <> other <> " not recognized."

      p (Object x) = x .: "amendment" >>= parseA x
      p x = fail $ "Value " <> show x <> " missing 'amendment' field."
  in unv1 "amendment" p 

