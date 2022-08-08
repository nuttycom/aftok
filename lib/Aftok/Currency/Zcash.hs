{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash
  ( Z.Address (..),
    Z._Address,
    Z.IVK (..),
    Z._IVK,
    RPCError (..),
    ZValidateAddressErr (..),
    ZcashdConfig (..),
    Z.Zatoshi (..),
    Z._Zatoshi,
    Z.ToZatoshi (..),
    Z.zsub,
    Z.Memo (..),
    rpcAddViewingKey,
    rpcValidateZAddr,
    getUserDiversifiedAddress,
  )
where

import Aftok.Currency.Zcash.Types as Z
import Aftok.Types (UserId)
import Control.Exception (catch)
import Control.Monad.Trans.Except (except)
import Data.Aeson (Value, encode, object, (.:), (.:?), (.=))
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

data ZcashdConfig = ZcashdConfig
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

data ZValidateAddressResp = ZValidateAddressResp
  { vzrIsValid :: Bool,
    --, vzrAddress  :: Maybe Text
    vzrAddrType :: Maybe Z.ZAddrType
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

parseAddrType :: A.Object -> Parser (Maybe Z.ZAddrType)
parseAddrType res = do
  typeStr <- res .:? "type"
  let typeMay = Z.decodeAddrType <$> typeStr
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

rpcValidateZAddr :: Manager -> ZcashdConfig -> Text -> IO (Either (RPCError ZValidateAddressErr) Z.Address)
rpcValidateZAddr mgr cfg addr = runExceptT $ do
  resp <- rpcEval mgr cfg (ZValidateAddress addr)
  except $
    if vzrIsValid resp
      then case vzrAddrType resp of
        Nothing -> Left (RPCError DataMissing)
        Just Z.Sprout -> Left (RPCError SproutAddress)
        Just Z.Sapling -> Right (Z.Address addr)
      else Left $ RPCError ZAddrInvalid

-- Viewing Keys

data ZImportViewingKeyResp = ZImportViewingKeyResp
  { addressType :: Z.ZAddrType
  -- , address :: Z.Address
  }

parseImportViewingKeyResponse :: Value -> Parser ZImportViewingKeyResp
parseImportViewingKeyResponse = \case
  (A.Object v) -> do
    ZImportViewingKeyResp
      <$> (maybe (fail "Missing address type.") pure =<< parseAddrType v)
  -- <*> (Z.Address <$> v .: "address")
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
    Z.Sprout -> Left . RPCError $ SproutViewingKey
    Z.Sapling -> Right ()

getUserDiversifiedAddress :: UserId -> IVK -> Address
getUserDiversifiedAddress = error "Not Yet Implemented."
