{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Json where

import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin
import Aftok.Currency.Zcash (_Zatoshi)
import Aftok.TimeLog
import Aftok.Types
import Control.Error.Util (maybeT)
import Control.FromSum
  ( fromEitherM,
  )
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.ByteString.Char8 as C
import Data.Data
import Data.HashMap.Strict as O
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Thyme.Clock as Clock
import Data.UUID as U
import Haskoin.Address
  ( textToAddr,
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

parseVersion :: MonadFail m => ByteString -> m Version
parseVersion = fromEitherM fail . PC.parseOnly versionParser

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
  ver <- parseVersion $ C.pack s
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

identifiedJSON :: Text -> Getter a UUID -> Getter a Value -> a -> Value
identifiedJSON name _id _value x =
  object [(name <> "Id") .= idValue _id x, name .= (x ^. _value)]

--
-- CreditTo
--

creditToJSON :: CreditTo -> Value
creditToJSON (CreditToAccount accountId) =
  object ["creditToAccount" .= idValue _AccountId accountId]
creditToJSON (CreditToUser uid) =
  object ["creditToUser" .= idValue _UserId uid]
creditToJSON (CreditToProject pid) =
  object ["creditToProject" .= idValue _ProjectId pid]

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
-- WorkIndex
--

amountJSON :: Amount -> Value
amountJSON (Amount currency value) = case currency of
  BTC -> object ["satoshi" .= (value ^. _Satoshi)]
  ZEC -> object ["zatoshi" .= (value ^. _Zatoshi)]

parseAmountJSON :: Value -> Parser Amount
parseAmountJSON = \case
  Object o ->
    maybeT (fail $ "Expected to find one of [\"satoshi\", \"zatoshi\"] as a key.") pure $
      MaybeT (fmap (Amount BTC . review _Satoshi) <$> o .:? "satoshi")
        <|> MaybeT (fmap (Amount ZEC . review _Zatoshi) <$> o .:? "zatoshi")
  val -> fail $ "Value " <> show val <> " is not a JSON object."

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
  identifiedJSON "billable" (_1 . B._BillableId) (_2 . to billableJSON)

recurrenceJSON' :: B.Recurrence -> Value
recurrenceJSON' B.Annually = object ["annually" .= Null]
recurrenceJSON' (B.Monthly i) = object ["monthly" .= object ["months" .= i]]
--recurrenceJSON' B.SemiMonthly = object [ "semimonthly" .= Null ]
recurrenceJSON' (B.Weekly i) = object ["weekly" .= object ["weeks" .= i]]
recurrenceJSON' B.OneTime = object ["onetime" .= Null]

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

parseLogEntry ::
  UserId ->
  (UTCTime -> LogEvent) ->
  Value ->
  Parser (UTCTime -> LogEntry)
parseLogEntry uid f = withObject "LogEntry" p
  where
    p o = do
      creditTo' <- o .:? "creditTo" >>= maybe (pure $ CreditToUser uid) (parseCreditToV2)
      eventMeta' <- o .:? "eventMeta"
      pure $ \t -> LogEntry creditTo' (f t) eventMeta'
