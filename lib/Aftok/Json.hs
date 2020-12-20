{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Json where

import qualified Aftok.Auction as A
import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin
import Aftok.Currency.Zcash (_Zatoshi)
import Aftok.Interval
import Aftok.Payments.Types
  ( PaymentId,
    _PaymentId,
  )
import qualified Aftok.Project as P
import Aftok.TimeLog
import Aftok.Types
import Control.FromSum
  ( fromEitherM,
  )
import Control.Lens hiding ((.=))
import qualified Control.Lens as L
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.ByteString.Char8 as C
import Data.Data
import Data.HashMap.Strict as O
import Data.List.NonEmpty as L
import Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Thyme.Calendar (showGregorian)
import Data.Thyme.Clock as Clock
import Data.Thyme.Time (Day)
import Data.UUID as U
import Haskoin.Address
  ( Address,
    textToAddr,
  )
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

data Version
  = Version
      { majorVersion :: Word8,
        minorVersion :: Word8
      }
  deriving (Typeable, Data)

failT :: Text -> Parser a
failT = fail . T.unpack

printVersion :: Version -> Text
printVersion Version {..} =
  T.intercalate "." $ fmap (T.pack . show) [majorVersion, minorVersion]

versionParser :: PC.Parser Version
versionParser = Version <$> PC.decimal <*> (PC.char '.' >> PC.decimal)

version :: MonadFail m => ByteString -> m Version
version = fromEitherM fail . PC.parseOnly versionParser

v :: QuasiQuoter
v =
  QuasiQuoter
    { quoteExp = quoteVersionExp,
      quotePat = error "Pattern quasiquotation of versions not supported.",
      quoteType = error "Type quasiquotation of versions not supported.",
      quoteDec = error "Dec quasiquotation of versions not supported."
    }

-- TODO: Include source location information, and implement quote patterns.
quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  ver <- version $ C.pack s
  dataToExpQ (const Nothing) ver

versioned :: Version -> Object -> Value
versioned ver o =
  Object $ uncurry O.insert ("schemaVersion" .= printVersion ver) o

-- |
-- - Convenience function to allow dispatch of different serialized
-- - versions to different parsers.
unversion :: String -> (Version -> Object -> Parser a) -> Value -> Parser a
unversion name f o = do
  verstr <- withObject name (.: "schemaVersion") o
  vers <- fromEitherM fail $ PC.parseOnly versionParser (T.encodeUtf8 verstr)
  withObject name (f vers) o

--------------
-- Versions --
--------------

v1 :: Object -> Value
v1 = versioned $ Version 1 0

v2 :: Object -> Value
v2 = versioned $ Version 2 0

unv1 :: String -> (Object -> Parser a) -> Value -> Parser a
unv1 name f = unversion name $ p
  where
    p (Version 1 0) = f
    p ver = badVersion name ver

badVersion :: forall v a. String -> Version -> v -> Parser a
badVersion name ver =
  const . fail $
    "Unrecognized " <> name <> " schema version: "
      <> T.unpack
        (printVersion ver)

-- convenience function to produce Object rather than Value
obj :: [Pair] -> Object
obj = O.fromList

-----------------
-- Serializers --
-----------------

idValue :: forall a. Getter a UUID -> a -> Value
idValue l a = toJSON . U.toText $ view l a

idJSON :: forall a. Text -> Getter a UUID -> a -> Value
idJSON t l a = v1 $ obj [t .= idValue l a]

qdbJSON :: Text -> Getter a UUID -> Getter a Value -> a -> Value
qdbJSON name _id _value x =
  v1 $ obj [(name <> "Id") .= idValue _id x, name .= (x ^. _value)]

projectIdJSON :: ProjectId -> Value
projectIdJSON = idJSON "projectId" _ProjectId

projectJSON :: P.Project -> Value
projectJSON p =
  v1 $
    obj
      [ "projectName" .= (p ^. P.projectName),
        "inceptionDate" .= (p ^. P.inceptionDate),
        "initiator" .= (p ^. P.initiator . _UserId)
      ]

qdbProjectJSON :: (ProjectId, P.Project) -> Value
qdbProjectJSON = qdbJSON "project" (_1 . _ProjectId) (_2 . L.to projectJSON)

auctionIdJSON :: A.AuctionId -> Value
auctionIdJSON = idJSON "auctionId" A._AuctionId

auctionJSON :: A.Auction -> Value
auctionJSON x =
  v1 $
    obj
      [ "projectId" .= idValue (A.projectId . _ProjectId) x,
        "initiator" .= idValue (A.initiator . _UserId) x,
        "raiseAmount" .= (x ^. (A.raiseAmount . _Satoshi))
      ]

bidIdJSON :: A.BidId -> Value
bidIdJSON pid = v1 $ obj ["bidId" .= (pid ^. A._BidId)]

--
-- CreditTo
--

creditToJSON :: CreditTo -> Value
creditToJSON (CreditToAccount accountId) =
  v2 $ obj ["creditToAccount" .= idValue _AccountId accountId]
creditToJSON (CreditToUser uid) =
  v2 $ obj ["creditToUser" .= idValue _UserId uid]
creditToJSON (CreditToProject pid) =
  v2 $ obj ["creditToProject" .= projectIdJSON pid]

parseCreditTo :: Value -> Parser CreditTo
parseCreditTo = unversion "CreditTo" $ \case
  (Version 2 0) -> parseCreditToV2
  ver -> badVersion "EventAmendment" ver

parseBtcAddr ::
  NetworkMode -> Text -> Parser Address
parseBtcAddr nmode addrText =
  maybe
    (fail . T.unpack $ "Address " <> addrText <> " cannot be parsed as a BTC network address.")
    pure
    (textToAddr (getNetwork nmode) addrText)

parseCreditToV2 :: Object -> Parser CreditTo
parseCreditToV2 o =
  let parseCreditToAcct = do
        fmap CreditToAccount . parseId _AccountId =<< o .: "creditToAccount"
      parseCreditToUser =
        fmap CreditToUser . parseId _UserId =<< o .: "creditToUser"
      parseCreditToProject =
        fmap CreditToProject . parseId _ProjectId =<< o .: "creditToProject"
      notFound =
        fail $ "Value " <> show o <> " does not represent a CreditTo value."
   in parseCreditToAcct
        <|> parseCreditToUser
        <|> parseCreditToProject
        <|> notFound

--
-- Payouts
--

payoutsJSON :: FractionalPayouts -> Value
payoutsJSON (Payouts m) =
  v2 $
    let payoutsRec :: (CreditTo, Rational) -> Value
        payoutsRec (c, r) =
          object ["creditTo" .= creditToJSON c, "payoutRatio" .= r]
     in obj $ ["payouts" .= fmap payoutsRec (MS.assocs m)]

parsePayoutsJSON :: Value -> Parser FractionalPayouts
parsePayoutsJSON = unversion "Payouts" $ p
  where
    p :: Version -> Object -> Parser FractionalPayouts
    p (Version 2 0) val =
      let parsePayoutRecord x =
            (,)
              <$> (parseCreditToV2 =<< (x .: "creditTo"))
              <*> (x .: "payoutRatio")
       in Payouts
            . MS.fromList
            <$> (traverse parsePayoutRecord =<< parseJSON (Object val))
    p ver x = badVersion "Payouts" ver x

--
-- WorkIndex
--

workIndexJSON :: WorkIndex -> Value
workIndexJSON (WorkIndex widx) =
  v2 $
    obj ["workIndex" .= fmap widxRec (MS.assocs widx)]
  where
    widxRec :: (CreditTo, NonEmpty Interval) -> Value
    widxRec (c, l) =
      object
        [ "creditTo" .= creditToJSON c,
          "intervals" .= (intervalJSON <$> L.toList l)
        ]

eventIdJSON :: EventId -> Value
eventIdJSON = idJSON "eventId" _EventId

logEventJSON' :: LogEvent -> Value
logEventJSON' ev =
  object [eventName ev .= object ["eventTime" .= (ev ^. eventTime)]]

logEntryJSON :: LogEntry -> Value
logEntryJSON le = v2 $ obj (logEntryFields le)

logEntryFields :: LogEntry -> [Pair]
logEntryFields (LogEntry c ev m) =
  [ "creditTo" .= creditToJSON c,
    "event" .= logEventJSON' ev,
    "eventMeta" .= m
  ]

amendmentIdJSON :: AmendmentId -> Value
amendmentIdJSON = idJSON "amendmentId" _AmendmentId

amountJSON :: Amount -> Value
amountJSON (Amount currency value) = case currency of
  BTC -> object ["satoshi" .= (value ^. _Satoshi)]
  ZEC -> object ["zatoshi" .= (value ^. _Zatoshi)]

billableIdJSON :: B.BillableId -> Value
billableIdJSON = idJSON "billableId" B._BillableId

billableJSON :: B.Billable Amount -> Value
billableJSON = v1 . obj . billableKV

billableKV :: (KeyValue kv) => B.Billable Amount -> [kv]
billableKV b =
  [ "projectId" .= idValue (B.project . _ProjectId) b,
    "name" .= (b ^. B.name),
    "description" .= (b ^. B.description),
    "recurrence" .= (b ^. B.recurrence . to recurrenceJSON'),
    "amount" .= (b ^. (B.amount . to amountJSON)),
    "gracePeriod" .= (b ^. B.gracePeriod),
    "requestExpiryPeriod" .= (b ^. B.requestExpiryPeriod . to Clock.toSeconds')
  ]

qdbBillableJSON :: (B.BillableId, B.Billable Amount) -> Value
qdbBillableJSON =
  qdbJSON "billable" (_1 . B._BillableId) (_2 . to billableJSON)

recurrenceJSON' :: B.Recurrence -> Value
recurrenceJSON' B.Annually = object ["annually" .= Null]
recurrenceJSON' (B.Monthly i) = object ["monthly " .= object ["months" .= i]]
--recurrenceJSON' B.SemiMonthly = object [ "semimonthly" .= Null ]
recurrenceJSON' (B.Weekly i) = object ["weekly " .= object ["weeks" .= i]]
recurrenceJSON' B.OneTime = object ["onetime" .= Null]

createSubscriptionJSON :: UserId -> B.BillableId -> Day -> Value
createSubscriptionJSON uid bid d =
  v1 $
    obj
      [ "user_id" .= idValue _UserId uid,
        "billable_id" .= idValue B._BillableId bid,
        "start_date" .= showGregorian d
      ]

subscriptionJSON :: B.Subscription -> Value
subscriptionJSON = v1 . obj . subscriptionKV

subscriptionKV :: (KeyValue kv) => B.Subscription -> [kv]
subscriptionKV sub =
  [ "user_id" .= idValue (B.customer . _UserId) sub,
    "billable_id" .= idValue (B.billable . B._BillableId) sub,
    "start_time" .= view B.startTime sub,
    "end_time" .= view B.endTime sub
  ]

subscriptionIdJSON :: B.SubscriptionId -> Value
subscriptionIdJSON = idJSON "subscriptionId" B._SubscriptionId

-- paymentRequestDetailsJSON :: [PaymentRequestDetail Amount] -> Value
-- paymentRequestDetailsJSON r = v1 $ obj ["payment_requests" .= fmap paymentRequestDetailJSON r]
--
-- paymentRequestDetailJSON :: PaymentRequestDetail Amount -> Object
-- paymentRequestDetailJSON r = obj $ concat
--   [ ["payment_request_id" .= view () r]
--   , paymentRequestKV $ view _2 r
--   , subscriptionKV $ view _3 r
--   , billableKV $ view _4 r
--   ]

paymentIdJSON :: PaymentId -> Value
paymentIdJSON = idJSON "paymentId" _PaymentId

-------------
-- Parsers --
-------------
parseUUID :: Value -> Parser U.UUID
parseUUID val = do
  str <- parseJSON val
  maybe (fail $ "Value " <> str <> "Could not be parsed as a valid UUID.") pure $
    U.fromString str

parseId :: forall a. Prism' a UUID -> Value -> Parser a
parseId p = fmap (review p) . parseUUID

parseEventAmendment ::
  ModTime ->
  Value ->
  Parser EventAmendment
parseEventAmendment t = unversion "EventAmendment" $ p
  where
    p (Version 2 0) = parseEventAmendmentV2 t
    p ver = badVersion "EventAmendment" ver

parseEventAmendmentV2 ::
  ModTime ->
  Object ->
  Parser EventAmendment
parseEventAmendmentV2 t o =
  let parseA :: Text -> Parser EventAmendment
      parseA "timeChange" = TimeChange t <$> o .: "eventTime"
      parseA "creditToChange" = CreditToChange t <$> parseCreditToV2 o
      parseA "metadataChange" = MetadataChange t <$> o .: "eventMeta"
      parseA tid =
        fail . T.unpack $ "Amendment type " <> tid <> " not recognized."
   in o .: "amendment" >>= parseA

parseLogEntry ::
  UserId ->
  (UTCTime -> LogEvent) ->
  Value ->
  Parser (UTCTime -> LogEntry)
parseLogEntry uid f = unversion "LogEntry" p
  where
    p (Version 2 0) o = do
      creditTo' <- o .:? "creditTo" >>= maybe (pure $ CreditToUser uid) (parseCreditToV2)
      eventMeta' <- o .:? "eventMeta"
      pure $ \t -> LogEntry creditTo' (f t) eventMeta'
    p ver o = badVersion "LogEntry" ver o

parseRecurrence :: Object -> Parser B.Recurrence
parseRecurrence o =
  let parseAnnually o' = const (pure B.Annually) <$> O.lookup "annually" o'
      parseMonthly o' = fmap B.Monthly . parseJSON <$> O.lookup "monthly" o'
      parseWeekly o' = fmap B.Weekly . parseJSON <$> O.lookup "weekly" o'
      parseOneTime o' = const (pure B.OneTime) <$> O.lookup "one-time" o'
      notFound =
        fail $ "Value " <> show o <> " does not represent a Recurrence value."
      parseV val =
        parseAnnually val
          <|> parseMonthly val
          <|> parseWeekly val
          <|> parseOneTime val
   in fromMaybe notFound $ parseV o

parseRecurrence' :: Value -> Parser B.Recurrence
parseRecurrence' (Object o) = parseRecurrence o
parseRecurrence' val = fail $ "Value " <> show val <> " is not a JSON object."
