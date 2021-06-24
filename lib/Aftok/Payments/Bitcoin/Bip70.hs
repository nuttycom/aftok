{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Bitcoin.Bip70
  ( module Bippy.Proto,
    protoBase64,
    fromBase64Proto,
  )
where

import Bippy.Proto
import qualified Data.ByteString.Base64 as B64
import Data.ProtocolBuffers (Decode, Encode, decodeMessage, encodeMessage)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)

protoBase64 :: Encode a => a -> Text
protoBase64 = B64.encodeBase64 . runPut . encodeMessage

fromBase64Proto :: Decode a => Text -> Either Text a
fromBase64Proto t = (first toText . runGet decodeMessage) <=< B64.decodeBase64 $ encodeUtf8 t
