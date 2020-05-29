{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}

module Aftok.Json where


import           Control.FromSum                ( fromMaybeM
                                                , fromEitherM
                                                )
import           Control.Lens            hiding ( (.=) )
import           Control.Monad.Fail             ( MonadFail(..) )
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8
                                               as PC
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as C
import           Data.Data
import           Data.HashMap.Strict           as O
import           Data.List.NonEmpty            as L
import           Data.Map.Strict               as MS
import           Data.ProtocolBuffers           ( encodeMessage )
import           Data.Serialize.Put             ( runPut )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Thyme.Calendar            ( showGregorian )
import           Data.Thyme.Clock              as Clock
import           Data.Thyme.Time                ( Day )
import           Data.UUID                     as U

import           Network.Haskoin.Address        ( Address
                                                , addrToJSON
                                                , addrFromJSON
                                                , stringToAddr
                                                )

import           Aftok.Currency.Bitcoin
import           Aftok.Auction                 as A
import qualified Aftok.Billables               as B
import           Aftok.Interval
import           Aftok.Payments
import           Aftok.Payments.Types           ( BillDetail )
import           Aftok.Project                 as P
import           Aftok.TimeLog
import           Aftok.Types
import           Aftok.Util                     ( traverseKeys )

import qualified Language.Haskell.TH           as TH
import           Language.Haskell.TH.Quote

data Version = Version { majorVersion :: Word8
                       , minorVersion :: Word8
                       } deriving (Typeable, Data)

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
v = QuasiQuoter
  { quoteExp  = quoteVersionExp
  , quotePat  = error "Pattern quasiquotation of versions not supported."
  , quoteType = error "Type quasiquotation of versions not supported."
  , quoteDec  = error "Dec quasiquotation of versions not supported."
  }

-- TODO: Include source location information, and implement quote patterns.
quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  ver <- version $ C.pack s
  dataToExpQ (const Nothing) ver

versioned :: Version -> Object -> Value
versioned ver o =
  Object $ uncurry O.insert ("schemaVersion" .= printVersion ver) o

{-|
 - Convenience function to allow dispatch of different serialized
 - versions to different parsers.
 -}
unversion :: String -> (Version -> Object -> Parser a) -> Value -> Parser a
unversion name f o = do
  verstr <- withObject name (.: "schemaVersion") o
  vers   <- fromEitherM fail $ PC.parseOnly versionParser (T.encodeUtf8 verstr)
  withObject name (f vers) o

--------------
-- Versions --
--------------

v1 :: Object -> Value
v1 = versioned $ Version 1 0

v2 :: Object -> Value
v2 = versioned $ Version 2 0

unv1 :: String -> (Object -> Parser a) -> Value -> Parser a
unv1 name f = unversion name $ p where
  p (Version 1 0) = f
  p ver           = badVersion name ver

badVersion :: forall v a . String -> Version -> v -> Parser a
badVersion name ver =
  const . fail $ "Unrecognized " <> name <> " schema version: " <> T.unpack
    (printVersion ver)

-- convenience function to produce Object rather than Value
obj :: [Pair] -> Object
obj = O.fromList

-----------------
-- Serializers --
-----------------

idValue :: forall a . Lens' a UUID -> a -> Value
idValue l a = toJSON . U.toText $ view l a

idJSON :: forall a . Text -> Lens' a UUID -> a -> Value
idJSON t l a = v1 $ obj [t .= idValue l a]

qdbJSON :: Text -> (Lens' a UUID) -> (b -> Value) -> (a, b) -> Value
qdbJSON name l f (xid, x) =
  v1 $ obj [(name <> "Id") .= idValue l xid, name .= f x]

projectIdJSON :: ProjectId -> Value
projectIdJSON = idJSON "projectId" _ProjectId

projectJSON :: Project -> Value
projectJSON p = v1 $ obj
  [ "projectName" .= (p ^. projectName)
  , "inceptionDate" .= (p ^. inceptionDate)
  , "initiator" .= (p ^. P.initiator . _UserId)
  ]

qdbProjectJSON :: (ProjectId, Project) -> Value
qdbProjectJSON = qdbJSON "project" _ProjectId projectJSON

auctionIdJSON :: AuctionId -> Value
auctionIdJSON = idJSON "auctionId" _AuctionId

auctionJSON :: Auction -> Value
auctionJSON x = v1 $ obj
  [ "projectId" .= idValue (A.projectId . _ProjectId) x
  , "initiator" .= idValue (A.initiator . _UserId) x
  , "raiseAmount" .= (x ^. (raiseAmount . satoshi))
  ]

bidIdJSON :: BidId -> Value
bidIdJSON pid = v1 $ obj ["bidId" .= (pid ^. _BidId)]

--
-- CreditTo
--

creditToJSON :: NetworkMode -> CreditTo (NetworkId, Address) -> Value
creditToJSON nmode (CreditToCurrency (netId, addr)) = v2 $ obj
  [ "creditToAddress" .= addrToJSON (toNetwork nmode netId) addr
  , "creditToNetwork" .= renderNetworkId netId
  ]
creditToJSON _ (CreditToUser uid) =
  v2 $ obj ["creditToUser" .= idValue _UserId uid]
creditToJSON _ (CreditToProject pid) =
  v2 $ obj ["creditToProject" .= projectIdJSON pid]

parseCreditTo :: NetworkMode -> Value -> Parser (CreditTo (NetworkId, Address))
parseCreditTo nmode = unversion "CreditTo" $ \case
  (Version 1 0) -> parseCreditToV1 nmode
  (Version 2 0) -> parseCreditToV2 nmode
  ver           -> badVersion "EventAmendment" ver

parseBtcAddr
  :: NetworkMode -> NetworkId -> Text -> Parser (CreditTo (NetworkId, Address))
parseBtcAddr nmode net addrText = maybe
  (  fail
  .  T.unpack
  $  "Address "
  <> addrText
  <> " cannot be parsed as a BTC network address."
  )
  (pure . CreditToCurrency . (net, ))
  (stringToAddr (toNetwork nmode net) addrText)

parseCreditToV1
  :: NetworkMode -> Object -> Parser (CreditTo (NetworkId, Address))
parseCreditToV1 nmode x = do
  parseBtcAddr nmode BTC =<< x .: "btcAddr"

parseCreditToV2
  :: NetworkMode -> Object -> Parser (CreditTo (NetworkId, Address))
parseCreditToV2 nmode o =
  let
    parseCreditToAddr = do
      netName <- o .: "creditToNetwork"
      net     <- fromMaybeM
        (fail . T.unpack $ "Currency network " <> netName <> " not recognized.")
        (parseNetworkId netName)
      addrValue <- o .: "creditToAddress"
      CreditToCurrency
        .   (net, )
        <$> addrFromJSON (toNetwork nmode net) addrValue

    parseCreditToUser =
      fmap CreditToUser . parseId _UserId =<< o .: "creditToUser"

    parseCreditToProject =
      fmap CreditToProject . parseId _ProjectId =<< o .: "creditToProject"

    notFound =
      fail $ "Value " <> show o <> " does not represent a CreditTo value."
  in
    parseCreditToAddr
    <|> parseCreditToUser
    <|> parseCreditToProject
    <|> notFound

--
-- Payouts
--

payoutsJSON :: NetworkMode -> Payouts (NetworkId, Address) -> Value
payoutsJSON nmode (Payouts m) =
  v2
    $ let payoutsRec :: (CreditTo (NetworkId, Address), Rational) -> Value
          payoutsRec (c, r) =
            object ["creditTo" .= creditToJSON nmode c, "payoutRatio" .= r]
      in  obj $ ["payouts" .= fmap payoutsRec (MS.assocs m)]

parsePayoutsJSON
  :: NetworkMode -> Value -> Parser (Payouts (NetworkId, Address))
parsePayoutsJSON nmode = unversion "Payouts" $ p where
  p :: Version -> Object -> Parser (Payouts (NetworkId, Address))
  p (Version 1 _) val = Payouts <$> join
    (traverseKeys (parseBtcAddr nmode BTC) <$> parseJSON (Object val))

  p (Version 2 0) val =
    let parsePayoutRecord x =
            (,)
              <$> (parseCreditToV2 nmode =<< (x .: "creditTo"))
              <*> (x .: "payoutRatio")
    in  Payouts
          .   MS.fromList
          <$> (traverse parsePayoutRecord =<< parseJSON (Object val))

  p ver x = badVersion "Payouts" ver x

--
-- WorkIndex
--

workIndexJSON :: NetworkMode -> WorkIndex (NetworkId, Address) -> Value
workIndexJSON nmode (WorkIndex widx) =
  v2
    $ let widxRec :: (CreditTo (NetworkId, Address), NonEmpty Interval) -> Value
          widxRec (c, l) = object
            [ "creditTo" .= creditToJSON nmode c
            , "intervals" .= (intervalJSON <$> L.toList l)
            ]
      in  obj $ ["workIndex" .= fmap widxRec (MS.assocs widx)]

eventIdJSON :: EventId -> Value
eventIdJSON = idJSON "eventId" _EventId

logEventJSON' :: LogEvent -> Value
logEventJSON' ev =
  object [eventName ev .= object ["eventTime" .= (ev ^. eventTime)]]

logEntryJSON :: NetworkMode -> LogEntry (NetworkId, Address) -> Value
logEntryJSON nmode (LogEntry c ev m) = v2 $ obj
  [ "creditTo" .= creditToJSON nmode c
  , "event" .= logEventJSON' ev
  , "eventMeta" .= m
  ]

amendmentIdJSON :: AmendmentId -> Value
amendmentIdJSON = idJSON "amendmentId" _AmendmentId

billableIdJSON :: B.BillableId -> Value
billableIdJSON = idJSON "billableId" B._BillableId

billableJSON :: B.Billable -> Value
billableJSON = v1 . obj . billableKV

billableKV :: (KeyValue kv) => B.Billable -> [kv]
billableKV b =
  [ "projectId" .= idValue (B.project . _ProjectId) b
  , "name" .= (b ^. B.name)
  , "description" .= (b ^. B.description)
  , "recurrence" .= recurrenceJSON' (b ^. B.recurrence)
  , "amount" .= (b ^. (B.amount . satoshi))
  , "gracePeriod" .= (b ^. B.gracePeriod)
  , "requestExpiryPeriod" .= (Clock.toSeconds' <$> (b ^. B.requestExpiryPeriod))
  ]

qdbBillableJSON :: (B.BillableId, B.Billable) -> Value
qdbBillableJSON = qdbJSON "billable" B._BillableId billableJSON

recurrenceJSON' :: B.Recurrence -> Value
recurrenceJSON' B.Annually    = object ["annually" .= Null]
recurrenceJSON' (B.Monthly i) = object ["monthly " .= object ["months" .= i]]
--recurrenceJSON' B.SemiMonthly = object [ "semimonthly" .= Null ]
recurrenceJSON' (B.Weekly  i) = object ["weekly " .= object ["weeks" .= i]]
recurrenceJSON' B.OneTime     = object ["onetime" .= Null]

createSubscriptionJSON :: UserId -> B.BillableId -> Day -> Value
createSubscriptionJSON uid bid d = v1 $ obj
  [ "user_id" .= idValue _UserId uid
  , "billable_id" .= idValue B._BillableId bid
  , "start_date" .= showGregorian d
  ]

subscriptionJSON :: B.Subscription -> Value
subscriptionJSON = v1 . obj . subscriptionKV

subscriptionKV :: (KeyValue kv) => B.Subscription -> [kv]
subscriptionKV sub =
  [ "user_id" .= idValue (B.customer . _UserId) sub
  , "billable_id" .= idValue (B.billable . B._BillableId) sub
  , "start_time" .= view B.startTime sub
  , "end_time" .= view B.endTime sub
  ]

subscriptionIdJSON :: B.SubscriptionId -> Value
subscriptionIdJSON = idJSON "subscriptionId" B._SubscriptionId

paymentRequestJSON :: PaymentRequest -> Value
paymentRequestJSON = v1 . obj . paymentRequestKV

paymentRequestKV :: (KeyValue kv) => PaymentRequest -> [kv]
paymentRequestKV r =
  [ "subscription_id" .= idValue (subscription . B._SubscriptionId) r
  , "payment_request_protobuf_64" .= view prBytes r
  , "url_key" .= view (paymentKey . _PaymentKey) r
  , "payment_request_time" .= view paymentRequestTime r
  , "billing_date" .= view (billingDate . to showGregorian) r
  ]
 where
  prBytes =
    paymentRequest . to (T.decodeUtf8 . B64.encode . runPut . encodeMessage)

billDetailsJSON :: [BillDetail] -> Value
billDetailsJSON r = v1 $ obj ["payment_requests" .= fmap billDetailJSON r]

billDetailJSON :: BillDetail -> Object
billDetailJSON r = obj $ concat
  [ ["payment_request_id" .= view (_1 . _PaymentKey) r]
  , paymentRequestKV $ view _2 r
  , subscriptionKV $ view _3 r
  , billableKV $ view _4 r
  ]

paymentIdJSON :: PaymentId -> Value
paymentIdJSON = idJSON "paymentId" _PaymentId

paymentJSON :: Payment -> Value
paymentJSON r = v1 $ obj
  [ "payment_request_id" .= idValue (request . _PaymentRequestId) r
  , "payment_protobuf_64" .= view paymentBytes r
  , "payment_date" .= (r ^. paymentDate)
  ]
 where
  paymentBytes =
    payment . to (T.decodeUtf8 . B64.encode . runPut . encodeMessage)

-------------
-- Parsers --
-------------
parseUUID :: Value -> Parser U.UUID
parseUUID val = do
  str <- parseJSON val
  maybe (fail $ "Value " <> str <> "Could not be parsed as a valid UUID.") pure
    $ U.fromString str

parseId :: forall a . Prism' a UUID -> Value -> Parser a
parseId p = fmap (review p) . parseUUID

parseEventAmendment
  :: NetworkMode
  -> ModTime
  -> Value
  -> Parser (EventAmendment (NetworkId, Address))
parseEventAmendment nmode t = unversion "EventAmendment" $ p where
  p (Version 1 _) = parseEventAmendmentV1 nmode t
  p (Version 2 0) = parseEventAmendmentV2 nmode t
  p ver           = badVersion "EventAmendment" ver

parseEventAmendmentV1
  :: NetworkMode
  -> ModTime
  -> Object
  -> Parser (EventAmendment (NetworkId, Address))
parseEventAmendmentV1 nmode t o =
  let
    parseA :: Text -> Parser (EventAmendment (NetworkId, Address))
    parseA "timeChange"     = TimeChange t <$> o .: "eventTime"
    parseA "addrChange"     = CreditToChange t <$> parseCreditToV1 nmode o
    parseA "metadataChange" = MetadataChange t <$> o .: "eventMeta"
    parseA tid =
      fail . T.unpack $ "Amendment type " <> tid <> " not recognized."
  in
    o .: "amendment" >>= parseA

parseEventAmendmentV2
  :: NetworkMode
  -> ModTime
  -> Object
  -> Parser (EventAmendment (NetworkId, Address))
parseEventAmendmentV2 nmode t o =
  let
    parseA :: Text -> Parser (EventAmendment (NetworkId, Address))
    parseA "timeChange"     = TimeChange t <$> o .: "eventTime"
    parseA "creditToChange" = CreditToChange t <$> parseCreditToV2 nmode o
    parseA "metadataChange" = MetadataChange t <$> o .: "eventMeta"
    parseA tid =
      fail . T.unpack $ "Amendment type " <> tid <> " not recognized."
  in
    o .: "amendment" >>= parseA

parseLogEntry
  :: NetworkMode
  -> UserId
  -> (UTCTime -> LogEvent)
  -> Value
  -> Parser (UTCTime -> (LogEntry (NetworkId, Address)))
parseLogEntry nmode uid f = unversion "LogEntry" p where
  p (Version 2 0) o = do
    creditTo' <- o .:? "creditTo" >>= maybe (pure $ CreditToUser uid)
                                            (parseCreditToV2 nmode)
    eventMeta' <- o .:? "eventMeta"
    pure $ \t -> LogEntry creditTo' (f t) eventMeta'

  p ver o = badVersion "LogEntry" ver o

parseRecurrence :: Object -> Parser B.Recurrence
parseRecurrence o =
  let
    parseAnnually o' = const (pure B.Annually) <$> O.lookup "annually" o'
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
  in
    fromMaybe notFound $ parseV o

parseRecurrence' :: Value -> Parser B.Recurrence
parseRecurrence' (Object o) = parseRecurrence o
parseRecurrence' val = fail $ "Value " <> show val <> " is not a JSON object."
