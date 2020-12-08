{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash
  ( ZAddr (..),
    _ZAddr,
    RPCError (..),
    ZValidateAddressErr (..),
    ZcashdConfig (..),
    Zatoshi,
    ToZatoshi (..),
    rpcAddViewingKey,
    rpcValidateZAddr,
  )
where

import Control.Exception (catch)
import Control.Lens (makePrisms)
import Control.Monad.Trans.Except (except)
import Data.Aeson ((.:), (.:?), (.=), Value, encode, object)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
  ( HttpException,
    Manager,
    RequestBody (..),
    applyBasicAuth,
    defaultRequest,
    host,
    httpLbs,
    method,
    port,
    requestBody,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Types (Status, statusCode)

coin :: Word64
coin = 100000000

maxMoney :: Word64
maxMoney = 21000000 * coin

newtype ZAddr = ZAddr {zaddrText :: Text}
  deriving (Eq, Ord, Show)

makePrisms ''ZAddr

newtype Zatoshi = Zatoshi Word64
  deriving (Eq, Ord, Show)

makePrisms ''Zatoshi

class ToZatoshi a where
  toZatoshi :: a -> Maybe Zatoshi

instance ToZatoshi Word64 where
  toZatoshi amt =
    if amt > maxMoney then Nothing else Just (Zatoshi amt)

data ZAddrType
  = Sprout
  | Sapling

data ZcashdConfig
  = ZcashdConfig
      { zcashdHost :: Text,
        zcashdPort :: Int,
        rpcUser :: Text,
        rpcPassword :: Text
      }

data RPCCall a where
  ZValidateAddress :: Text -> RPCCall ZValidateAddressResp
  ZImportViewingKey :: Text -> RPCCall ZImportViewingKeyResp

data RPCError e
  = HttpError HttpException
  | ServiceError Status
  | ParseError String
  | RPCError e
  deriving (Show)

toRequestBody :: RPCCall a -> Value
toRequestBody = \case
  ZValidateAddress addr -> validateZAddrRequest addr
  ZImportViewingKey vk -> importViewingKeyRequest vk

rpcEval :: A.FromJSON a => Manager -> ZcashdConfig -> RPCCall a -> ExceptT (RPCError e) IO a
rpcEval mgr cfg call = do
  let req =
        applyBasicAuth (T.encodeUtf8 $ rpcUser cfg) (T.encodeUtf8 $ rpcPassword cfg) $
          defaultRequest
            { host = T.encodeUtf8 $ zcashdHost cfg,
              port = zcashdPort cfg,
              method = "POST",
              requestBody = RequestBodyLBS . encode $ toRequestBody call
            }
  response <-
    ExceptT $
      catch
        (Right <$> httpLbs req mgr)
        (pure . Left . HttpError)
  let status = responseStatus response
  except $ case statusCode status of
    200 -> first ParseError $ A.eitherDecode (responseBody response)
    _ -> Left (ServiceError status)

-- Address Validation

data ZValidateAddressErr
  = ZAddrInvalid
  | SproutAddress
  | DataMissing
  deriving (Eq, Show)

data ZValidateAddressResp
  = ZValidateAddressResp
      { vzrIsValid :: Bool,
        --, vzrAddress  :: Maybe Text
        vzrAddrType :: Maybe ZAddrType
      }

instance A.FromJSON ZValidateAddressResp where
  parseJSON = parseValidateZAddrResponse

validateZAddrRequest :: Text -> Value
validateZAddrRequest addr =
  object
    [ "jsonrpc" .= ("1.0" :: Text),
      "id" .= ("aftok-z_validateaddress" :: Text),
      "method" .= ("z_validateaddress" :: Text),
      "params" .= [addr]
    ]

decodeAddrType :: Text -> Maybe ZAddrType
decodeAddrType = \case
  "sprout" -> Just Sprout
  "sapling" -> Just Sapling
  _ -> Nothing

parseAddrType :: A.Object -> Parser (Maybe ZAddrType)
parseAddrType res = do
  typeStr <- res .:? "type"
  let typeMay = decodeAddrType <$> typeStr
  traverse (maybe (fail $ "Not a recognized zaddr type: " <> show typeStr) pure) typeMay

parseValidateZAddrResponse :: Value -> Parser ZValidateAddressResp
parseValidateZAddrResponse = \case
  (A.Object v) -> do
    res <- v .: "result"
    ZValidateAddressResp <$> res .: "isvalid"
      -- <*> res .:? "address"
      <*> parseAddrType res
  _ ->
    fail "ZAddr validation response body was not a valid JSON object"

rpcValidateZAddr :: Manager -> ZcashdConfig -> Text -> IO (Either (RPCError ZValidateAddressErr) ZAddr)
rpcValidateZAddr mgr cfg addr = runExceptT $ do
  resp <- rpcEval mgr cfg (ZValidateAddress addr)
  except $
    if vzrIsValid resp
      then case vzrAddrType resp of
        Nothing -> Left (RPCError DataMissing)
        Just Sprout -> Left (RPCError SproutAddress)
        Just Sapling -> Right (ZAddr addr)
      else Left $ RPCError ZAddrInvalid

-- Viewing Keys

data ZImportViewingKeyResp
  = ZImportViewingKeyResp
      { addressType :: ZAddrType
        -- , address :: ZAddr
      }

parseImportViewingKeyResponse :: Value -> Parser ZImportViewingKeyResp
parseImportViewingKeyResponse = \case
  (A.Object v) -> do
    ZImportViewingKeyResp
      <$> (maybe (fail "Missing address type.") pure =<< parseAddrType v)
  -- <*> (ZAddr <$> v .: "address")
  _ ->
    fail "z_importviewingkey response body was not a valid JSON object"

instance A.FromJSON ZImportViewingKeyResp where
  parseJSON = parseImportViewingKeyResponse

data ZImportViewingKeyError
  = SproutViewingKey

importViewingKeyRequest :: Text -> Value
importViewingKeyRequest vk =
  object
    [ "jsonrpc" .= ("1.0" :: Text),
      "id" .= ("aftok-z_importviewingkey" :: Text),
      "method" .= ("z_importviewingkey" :: Text),
      "params" .= [vk, "no"] -- no need to rescan, for our purposes
    ]

rpcAddViewingKey :: Manager -> ZcashdConfig -> Text -> IO (Either (RPCError ZImportViewingKeyError) ())
rpcAddViewingKey mgr cfg vk = runExceptT $ do
  resp <- rpcEval mgr cfg (ZImportViewingKey vk)
  except $ case addressType resp of
    Sprout -> Left . RPCError $ SproutViewingKey
    Sapling -> Right ()
