{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Database.PostgreSQL.Types
  ( DBM,
    SerDepFunction (..),
    pexec,
    pinsert,
    pquery,
    ptransact,
    askNetworkMode,
    idParser,
    utcParser,
    nullField,
    nominalDiffTimeParser,
    creditToParser,
    creditToName,
    bitcoinAddressParser,
    zcashAddressParser,
    zcashIvkParser,
    currencyAmountParser,
    btcAmountParser,
    zecAmountParser,
    currencyType,
    currencyValue,
  )
where

import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin (Satoshi (..), _Satoshi)
import qualified Aftok.Currency.Bitcoin as Bitcoin
import Aftok.Currency.Zcash (Zatoshi (..), _Zatoshi)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database (DBError)
import Aftok.TimeLog.Serialization
  ( depfFromJSON,
    depfToJSON,
  )
import Aftok.Types
  ( AccountId (..),
    CreditTo (..),
    DepreciationFunction,
    ProjectId (..),
    UserId (..),
  )
import Control.Lens ((^.))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import qualified Data.List as L
import qualified Data.Text as T
import Data.Thyme.Clock as C
import Data.Thyme.Time as C
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
  ( Connection,
    Query,
    ResultError (Incompatible),
    ToRow,
    execute,
    fromOnly,
    query,
    queryWith,
    withTransaction,
  )
import Database.PostgreSQL.Simple.FromField
  ( FieldParser,
    ResultError (ConversionFailed),
    fromField,
    returnError,
    typename,
  )
import Database.PostgreSQL.Simple.FromRow (RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Types (Null)
import qualified Haskoin.Address as Bitcoin
import qualified Haskoin.Constants as Bitcoin

newtype SerDepFunction = SerDepFunction {unSerDepFunction :: DepreciationFunction}

instance FromJSON SerDepFunction where
  parseJSON v = SerDepFunction <$> depfFromJSON v

instance ToJSON SerDepFunction where
  toJSON (SerDepFunction depf) = depfToJSON depf

type DBM a = ReaderT (Bitcoin.NetworkMode, Connection) (ExceptT DBError IO) a

pexec :: (ToRow d) => Query -> d -> DBM Int64
pexec q d = do
  conn <- asks snd
  lift . lift $ execute conn q d

pinsert :: (ToRow d) => (UUID -> r) -> Query -> d -> DBM r
pinsert f q d = do
  conn <- asks snd
  ids <- lift . lift $ query conn q d
  pure . f . fromOnly $ L.head ids

pquery :: (ToRow d) => RowParser r -> Query -> d -> DBM [r]
pquery p q d = do
  conn <- asks snd
  lift . lift $ queryWith p conn q d

ptransact :: DBM a -> DBM a
ptransact rt = do
  env <- ask
  lift . ExceptT $ withTransaction (snd env) (runExceptT $ runReaderT rt env)

askNetworkMode :: DBM Bitcoin.NetworkMode
askNetworkMode = asks fst

idParser :: (UUID -> a) -> RowParser a
idParser f = f <$> field

utcParser :: RowParser C.UTCTime
utcParser = C.toThyme <$> field

nullField :: RowParser Null
nullField = field

nominalDiffTimeParser :: FieldParser NominalDiffTime
nominalDiffTimeParser f v = C.fromSeconds' <$> fromField f v

creditToName :: CreditTo -> Text
creditToName (CreditToAccount _) = "credit_to_account"
creditToName (CreditToUser _) = "credit_to_user"
creditToName (CreditToProject _) = "credit_to_project"

creditToParser :: RowParser CreditTo
creditToParser = join $ fieldWith creditToParser'

creditToParser' :: FieldParser (RowParser CreditTo)
creditToParser' f v = do
  tn <- typename f
  if tn /= "credit_to_t"
    then returnError Incompatible f "column was not of type credit_to_t"
    else maybe empty (pure . parser . decodeUtf8) v
  where
    parser :: Text -> RowParser CreditTo
    parser = \case
      "credit_to_account" ->
        CreditToAccount <$> (idParser AccountId <* nullField <* nullField)
      "credit_to_user" ->
        CreditToUser <$> (nullField *> idParser UserId <* nullField)
      "credit_to_project" ->
        CreditToProject
          <$> (nullField *> nullField *> idParser ProjectId)
      _ -> empty

bitcoinAddressParser :: Bitcoin.NetworkMode -> RowParser Bitcoin.Address
bitcoinAddressParser nmode =
  fieldWith $ addrFieldParser (Bitcoin.getNetwork nmode)
  where
    addrFieldParser :: Bitcoin.Network -> FieldParser Bitcoin.Address
    addrFieldParser n f v = do
      fieldValue <- fromField f v
      let addrMay = Bitcoin.textToAddr n fieldValue
      let err =
            returnError
              ConversionFailed
              f
              ( "could not deserialize value "
                  <> T.unpack fieldValue
                  <> " to a valid BTC address for network "
                  <> show n
              )
      maybe err pure addrMay

btcAmountParser :: RowParser Satoshi
btcAmountParser = (Satoshi . fromInteger) <$> field

zecAmountParser :: RowParser Zatoshi
zecAmountParser = (Zatoshi . fromInteger) <$> field

currencyAmountParser :: RowParser Amount
currencyAmountParser = join $ fieldWith currencyAmountParser'

currencyAmountParser' :: FieldParser (RowParser Amount)
currencyAmountParser' f v = do
  tn <- typename f
  if tn /= "currency_t"
    then returnError Incompatible f "column was not of type currency_t"
    else maybe empty (pure . parser . decodeUtf8) v
  where
    parser :: Text -> RowParser Amount
    parser = \case
      "ZEC" -> Amount ZEC <$> zecAmountParser
      "BTC" -> Amount BTC <$> btcAmountParser
      _ -> empty

-- TODO: address validation here?
zcashAddressParser :: RowParser Zcash.Address
zcashAddressParser = Zcash.Address <$> field

-- TODO: ivk validation here?
zcashIvkParser :: RowParser Zcash.IVK
zcashIvkParser = Zcash.IVK <$> field

currencyType :: Amount -> Text
currencyType = \case
  Amount BTC _ -> "BTC"
  Amount ZEC _ -> "ZEC"

currencyValue :: Amount -> Word64
currencyValue = \case
  Amount BTC sats -> sats ^. _Satoshi
  Amount ZEC zats -> zats ^. _Zatoshi
