{-# LANGUAGE TemplateHaskell    #-}

module Aftok.Currency.Zcash
  ( ZAddr(..)
  , _ZAddr
  , ZAddrError(..)
  , ZcashdConfig(..)
  , rpcValidateZAddr
  ) where

import           Control.Lens                   ( makePrisms )

import qualified Data.Aeson                     as A
import           Data.Aeson                     ( Value, (.=), (.:), (.:?), object, encode )
import           Data.Aeson.Types               ( Parser )
import qualified Data.Text.Encoding            as T

import           Network.HTTP.Client            ( Manager
                                                , RequestBody(..)
                                                , defaultRequest
                                                , responseBody
                                                , responseStatus
                                                , httpLbs
                                                , host, port, method, requestBody
                                                )
import           Network.HTTP.Types             ( Status, statusCode )

newtype ZAddr = ZAddr { zaddrText :: Text }
  deriving (Eq, Ord, Show)
makePrisms ''ZAddr

data ZAddrType
  = Sprout
  | Sapling

data ZcashdConfig = ZcashdConfig
  { zcashdHost :: Text
  , zcashdPort :: Int
  }

data ZAddrError
  = ServiceError Status
  | ParseError String
  | ZAddrInvalid
  | SproutAddress
  | DataMissing

validateZAddrRequest :: Text -> Value
validateZAddrRequest addr = object
  [ "jsonrpc" .= ("1.0" :: Text)
  , "id"      .= ("aftok-z_validateaddress" :: Text)
  , "method"  .= ("z_validateaddress" :: Text)
  , "params"  .= [addr]
  ]

data ValidateZAddrResponse = ValidateZAddrResponse
  { isValid :: Bool
  , _address :: Maybe Text
  , addrType :: Maybe ZAddrType
  }

instance A.FromJSON ValidateZAddrResponse where
  parseJSON = parseValidateZAddrResponse

parseAddrType :: Text -> Maybe ZAddrType
parseAddrType = \case
  "sprout" -> Just Sprout
  "sapling" -> Just Sapling
  _ -> Nothing

parseValidateZAddrResponse :: Value -> Parser ValidateZAddrResponse
parseValidateZAddrResponse = \case
  (A.Object v) ->
    ValidateZAddrResponse <$> v .: "isvalid"
                          <*> v .:? "address"
                          <*> ((traverse (maybe (fail "Not a recognized zaddr type") pure) . fmap parseAddrType) =<< v .:? "type")

  _ ->
    fail "ZAddr validation response body was not a valid JSON object"


rpcValidateZAddr :: Manager -> ZcashdConfig -> Text -> IO (Either ZAddrError ZAddr)
rpcValidateZAddr mgr cfg addr = do
  let req = defaultRequest { host = T.encodeUtf8 $ zcashdHost cfg
                           , port = zcashdPort cfg
                           , method = "POST"
                           , requestBody = RequestBodyLBS $ encode (validateZAddrRequest addr)
                           }

  response <- httpLbs req mgr
  let status = responseStatus response
  pure $ case statusCode status of
    200 ->
      case A.eitherDecode (responseBody response) of
        Left err -> Left (ParseError err)
        Right resp ->
          if isValid resp
            then
              case addrType resp of
                Just Sprout -> Left SproutAddress
                Just Sapling -> Right (ZAddr addr)
                _ -> Left DataMissing
            else
              Left ZAddrInvalid
    _ ->
      Left (ServiceError status)
