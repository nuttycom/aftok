{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Aftok.Json where

import           ClassyPrelude

import           Control.Lens                     hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.ByteString.Char8            as C
import           Data.Data
import           Data.List.NonEmpty               as L
import           Data.Map.Strict                  as MS
import           Data.HashMap.Strict              as O
import           Data.UUID                        as U

import           Aftok
import           Aftok.Auction                    as A
import           Aftok.Database
import           Aftok.Interval
import           Aftok.Project                    as P
import           Aftok.TimeLog
import           Aftok.Types

import qualified Language.Haskell.TH              as TH
import           Language.Haskell.TH.Quote

data Version = Version { majorVersion :: Word8
                       , minorVersion :: Word8
                       } deriving (Typeable, Data)

instance Show Version where
  show Version{..} = intercalate "." $ fmap show [majorVersion, minorVersion]

versionParser :: PC.Parser Version
versionParser = Version <$> PC.decimal <*> (PC.char '.' >> PC.decimal)

version :: QuasiQuoter
version = QuasiQuoter { quoteExp = quoteVersionExp
                      , quotePat = error "Pattern quasiquotation of versions not supported."
                      , quoteType = error "Type quasiquotation of versions not supported."
                      , quoteDec = error "Dec quasiquotation of versions not supported."
                      }

-- TODO: Include source location information, and implement quote patterns.
quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  v <- either (fail . show) pure $ PC.parseOnly versionParser (C.pack s)
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
  verstr <- v .: "schemaVersion"
  vers   <- either fail pure $ PC.parseOnly versionParser (encodeUtf8 verstr)
  v .: "value" >>= f vers

unversion _ x =
  fail $ show x <> " did not contain the expected version information."

--------------
-- Versions --
--------------

v1 :: Value -> Value
v1 = versioned $ Version 1 0

v2 :: Value -> Value
v2 = versioned $ Version 2 0

unv1 :: String -> (Value -> Parser a) -> Value -> Parser a
unv1 name f = unversion $ \x -> case x of
  Version 1 0 -> f
  _           -> badVersion name x

badVersion :: String -> Version -> Value -> Parser a
badVersion name ver = const . fail $ "Unrecognized " <> name <> " schema version: " <> show ver

-----------------
-- Serializers --
-----------------

qdbProjectJSON :: KeyedProject -> Value
qdbProjectJSON (pid, project) = v1 $
  object [ "projectId" .=  tshow (pid ^. _ProjectId)
         , "project" .= projectJSON project
         ]

projectIdJSON :: ProjectId -> Value
projectIdJSON pid = v1 $
  object [ "projectId" .= tshow (pid ^. _ProjectId) ]

projectJSON :: Project -> Value
projectJSON p = v1 $
  object [ "projectName"    .= (p ^. projectName)
         , "inceptionDate"  .= (p ^. inceptionDate)
         , "initiator"      .= tshow (p ^. (P.initiator._UserId))
         ]

auctionIdJSON :: AuctionId -> Value
auctionIdJSON pid = v1 $
  object [ "auctionId" .= tshow (pid ^. _AuctionId) ]

auctionJSON :: Auction -> Value
auctionJSON x = v1 $
  object [ "projectId"    .= tshow (x ^. (A.projectId._ProjectId))
         , "initiator"    .= tshow (x ^. (A.initiator._UserId))
         , "raiseAmount"  .= (x ^. (raiseAmount._Satoshi))
         ]

bidIdJSON :: BidId -> Value
bidIdJSON pid = v1 $
  object [ "bidId" .= tshow (pid ^. _BidId) ]

creditToJSON :: CreditTo -> Value
creditToJSON (CreditToAddress addr) = v2 $ object [ "creditToAddress" .= (addr ^. _BtcAddr) ]
creditToJSON (CreditToUser uid)     = v2 $ object [ "creditToUser"    .= tshow (uid ^. _UserId) ]
creditToJSON (CreditToProject pid)  = v2 $ object [ "creditToProject" .= projectIdJSON pid ]

payoutsJSON :: Payouts -> Value
payoutsJSON (Payouts m) = v2 $
  let payoutsRec :: (CreditTo, Rational) -> Value
      payoutsRec (c, r) = object [ "creditTo" .= creditToJSON c
                                 , "payoutRatio" .= r
                                 ]
  in  toJSON $ fmap payoutsRec (MS.assocs m)

workIndexJSON :: WorkIndex -> Value
workIndexJSON (WorkIndex widx) = v2 $
  let widxRec :: (CreditTo, NonEmpty Interval) -> Value
      widxRec (c, l) = object [ "creditTo"  .= creditToJSON c
                              , "intervals" .= (intervalJSON <$> L.toList l)
                              ]
  in  toJSON $ fmap widxRec (MS.assocs widx)

eventIdJSON :: EventId -> Value
eventIdJSON (EventId eid) = v1 $
  object [ "eventId" .= tshow eid ]


logEventJSON :: LogEvent -> Value
logEventJSON ev = object [ eventName ev .= object [ "eventTime" .= (ev ^. eventTime) ] ]

logEntryJSON :: LogEntry -> Value
logEntryJSON (LogEntry c ev m) = v2 $ 
  object [ "creditTo"  .= creditToJSON c
         , "event" .= logEventJSON ev
         , "eventMeta" .= m
         ]

amendmentIdJSON :: AmendmentId -> Value
amendmentIdJSON (AmendmentId aid) = v1 $
  object [ "amendmentId" .= tshow aid ]

-------------
-- Parsers --
-------------

parsePayoutsJSON :: Value -> Parser Payouts
parsePayoutsJSON = unversion $ \ver -> case ver of
  (Version 1 _) -> \v -> Payouts . MS.mapKeys (CreditToAddress . BtcAddr) <$> parseJSON v 
  (Version 2 0) -> \v -> do
    xs <- parseJSON v
    let parsePayoutRecord x = (,) <$> (parseCreditTo =<< (x .: "creditTo"))
                                  <*> x .: "payoutRatio"
    Payouts . MS.fromList <$> traverse parsePayoutRecord xs 
  _             -> badVersion "Payouts" ver

parseEventAmendment :: ModTime -> Value -> Parser EventAmendment
parseEventAmendment t = unversion $ \v -> case v of
  Version 1 0 -> parseEventAmendmentV1 t
  Version 2 0 -> parseEventAmendmentV2 t
  _           -> badVersion "EventAmendment" v

parseEventAmendmentV1 :: ModTime -> Value -> Parser EventAmendment
parseEventAmendmentV1 t v@(Object x) =
  let parseA :: Text -> Parser EventAmendment
      parseA "timeChange"     = TimeChange t <$> x .: "eventTime"
      parseA "addrChange"     = CreditToChange t <$> parseCreditTo v
      parseA "metadataChange" = MetadataChange t <$> x .: "eventMeta"
      parseA tid = fail . show $ "Amendment type " <> tid <> " not recognized."
  in  x .: "amendment" >>= parseA 

parseEventAmendmentV1 _ x =
  fail $ "Value " <> show x <> " is not a JSON object."

parseEventAmendmentV2 :: ModTime -> Value -> Parser EventAmendment
parseEventAmendmentV2 t v@(Object x) =
  let parseA :: Text -> Parser EventAmendment
      parseA "timeChange"     = TimeChange t <$> x .: "eventTime"
      parseA "creditToChange" = CreditToChange t <$> parseCreditTo v
      parseA "metadataChange" = MetadataChange t <$> x .: "eventMeta"
      parseA tid = fail . show $ "Amendment type " <> tid <> " not recognized."
  in  x .: "amendment" >>= parseA 

parseEventAmendmentV2 _ x =
  fail $ "Value " <> show x <> " is not a JSON object."

parseBtcAddrJson :: Value -> Parser BtcAddr
parseBtcAddrJson v = do
  t <- parseJSON v
  maybe (fail $ show t <> " is not a valid BTC address") pure $ parseBtcAddr t

parseUUID :: Value -> Parser U.UUID
parseUUID v = do
  str <- parseJSON v
  maybe (fail $ "Value " <> str <> "Could not be parsed as a valid UUID.") pure $ U.fromString str

parseCreditTo :: Value -> Parser CreditTo
parseCreditTo = unversion $ \v -> case v of
  Version 1 0 -> withObject "BtcAddr"  parseCreditToV1
  Version 2 0 -> withObject "CreditTo" parseCreditToV2
  _           -> badVersion "EventAmendment" v

parseCreditToV1 :: Object -> Parser CreditTo 
parseCreditToV1 x = CreditToAddress <$> (parseBtcAddrJson =<< (x .: "btcAddr"))

parseCreditToV2 :: Object -> Parser CreditTo
parseCreditToV2 x = 
  let parseCreditToAddr (Object x') = do 
        addrText <- O.lookup "creditToAddress" x'
        pure (CreditToAddress <$> parseBtcAddrJson addrText)
      parseCreditToAddr _ = Nothing

      parseCreditToUser (Object x') = do
        userText <- O.lookup "creditToUser" x'
        pure (CreditToUser . UserId <$> parseUUID userText)
      parseCreditToUser _ = Nothing

      --parseCreditToProject (Object x') = Nothing
      parseCreditToProject _ = Nothing

      notFound = fail $ "Value " <> show x <> " does not represent a CreditTo value."
      parseV v = (parseCreditToAddr v <|> parseCreditToUser v <|> parseCreditToProject v)

  in  do
    body <- x .: "creditTo"
    fromMaybe notFound $ parseV body 

parseLogEvent :: Object -> Parser LogEvent
parseLogEvent x = 
  (StartWork <$> x .: "start") <|> (StopWork <$> x .: "stop")

parseLogEntry :: Value -> Parser LogEntry
parseLogEntry = unversion parseLogEntry' where 
  parseLogEntry' (Version 2 0) (Object x) =
    LogEntry <$> (x .: "creditTo" >>= parseCreditTo)
             <*> (x .: "event" >>= parseLogEvent)
             <*> (x .: "eventMeta")
  parseLogEntry' v x = badVersion "LogEntry" v x

