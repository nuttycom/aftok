{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash.Zip321 where

import Aftok.Currency.Zcash.Types
import Control.Lens ((^.), makeLenses, makePrisms)
import Data.Attoparsec.Text
  ( Parser,
    char,
    choice,
    decimal,
    option,
    parseOnly,
    scientific,
    sepBy1,
    string,
    takeText,
    takeTill,
    takeWhile1,
  )
import Data.ByteString.Base64.URL (decodeBase64, encodeBase64Unpadded)
import Data.Char (isAlpha, isAscii, isDigit)
import Data.List.NonEmpty (zip)
import qualified Data.Map.Strict as M
import Data.Scientific (toBoundedInteger)
import Data.Text (any, intercalate, pack, unpack)
import Network.URI.Encode (decodeText, encodeTextWith)
import Text.Printf (printf)
import Prelude hiding (any, intercalate, zip)

data PaymentItem
  = PaymentItem
      { _address :: Address,
        _amount :: Zatoshi,
        _memo :: Maybe Memo,
        _message :: Maybe Text,
        _label :: Maybe Text,
        _other :: [(Text, Text)] -- TODO: param name restrictions
      }

makeLenses ''PaymentItem

data PaymentRequest
  = PaymentRequest
      { _items :: NonEmpty PaymentItem
      }

makeLenses ''PaymentRequest

-- The set of ASCII characters that are excepted from percent-encoding according
-- to the definition of ZIP 321.
--
--       unreserved      = ALPHA / DIGIT / "-" / "." / "_" / "~"
--       allowed-delims  = "!" / "$" / "'" / "(" / ")" / "*" / "+" / "," / ";"
--       qchar           = unreserved / pct-encoded / allowed-delims / ":" / "@"
qchar :: Char -> Bool
qchar c =
  (isAscii c && isAlpha c)
    || isDigit c
    || any (== c) "-._!$'()*+,;:@"

paramIndex :: Maybe Int -> Text
paramIndex = \case
  Just i | i > 0 -> pack (printf ".%d" i)
  _ -> ""

addrParam :: Maybe Int -> Address -> Text
addrParam i (Address t) = strParam "address" i t

amountParam :: Maybe Int -> Zatoshi -> Text
amountParam i (Zatoshi value) =
  "amount" <> paramIndex i <> "=" <> valueText
  where
    coins = value `div` coin
    zats = value `mod` coin
    valueText =
      pack $
        if zats == 0
          then printf "%d" coins
          else printf "%d.%0.8d" coins zats

strParam :: Text -> Maybe Int -> Text -> Text
strParam l i value =
  l <> paramIndex i <> "=" <> encodeTextWith qchar value

memoParam :: Maybe Int -> Memo -> Text
memoParam i (Memo bytes) = "memo" <> paramIndex i <> "=" <> encodeBase64Unpadded bytes

itemPartial :: Maybe Int -> PaymentItem -> [Text]
itemPartial i item =
  catMaybes
    [ Just $ amountParam i (item ^. amount),
      memoParam i <$> (item ^. memo),
      strParam "message" i <$> (item ^. message),
      strParam "label" i <$> (item ^. label)
    ]

itemsParams :: NonEmpty PaymentItem -> NonEmpty Text
itemsParams xs =
  intercalate "&" . toList . itemParams <$> zip (Just <$> fromList [0 ..]) xs
  where
    itemParams (i, item) =
      addrParam i (item ^. address) : itemPartial i item

toURI :: PaymentRequest -> Text
toURI req =
  case req ^. items of
    i :| [] ->
      "zcash:" <> zaddrText (i ^. address) <> "?"
        <> intercalate "&" (itemPartial Nothing i)
    xs ->
      "zcash:?" <> intercalate "&" (toList $ itemsParams xs)

addrElem :: Char -> Bool
addrElem c = isDigit c || (isAscii c && isAlpha c)

data Zip321Param
  = AddrParam Address
  | AmountParam Zatoshi
  | MemoParam Memo
  | LabelParam Text
  | MessageParam Text
  | OtherParam Text Text

makePrisms ''Zip321Param

type IndexedParam = (Int, Zip321Param)

zip321Parser :: Parser PaymentRequest
zip321Parser = do
  void $ string "zcash:"
  addr0 <- toAddress <$> takeTill (== '?')
  params' <- sepBy1 zip321Param (char '&')
  let params = second (: []) <$> (toList addr0 <> params')
      grouped = M.fromListWith (<>) params
  groups <- maybe (fail "Parameter list was empty.") pure (nonEmpty $ M.toAscList grouped)
  either (fail . unpack) (pure . PaymentRequest) $ traverse (toPaymentItem . snd) groups
  where
    toAddress addr =
      if addr == ""
        then Nothing
        else Just (0, AddrParam $ Address addr)
    zip321Param =
      choice
        [ parseAddrParam,
          parseAmountParam,
          parseMemoParam,
          parseLabelParam,
          parseMessageParam,
          parseOtherParam
        ]
    toPaymentItem :: [Zip321Param] -> Either Text PaymentItem
    toPaymentItem = error "Not yet implemented." --PaymentItem <$> note "Payment address is required"

indexedParam :: Text -> Parser Zip321Param -> Parser IndexedParam
indexedParam name valuep = do
  void $ string name
  idx <- option 0 (char '.' *> decimal)
  (,) <$> pure idx <*> (char '=' *> valuep)

parseAddrParam :: Parser IndexedParam
parseAddrParam = indexedParam "address" (AddrParam . Address <$> takeWhile1 addrElem)

parseAmountParam :: Parser IndexedParam
parseAmountParam = indexedParam "amount" $ do
  s <- scientific
  let zats = s * fromIntegral coin
  maybe
    (fail "Amount is out of bounds")
    (pure . AmountParam . Zatoshi)
    (toBoundedInteger zats)

parseMemoParam :: Parser IndexedParam
parseMemoParam = indexedParam "memo" $ do
  t <- takeText
  either
    (\e -> fail . unpack $ "Base64 decoding of memo value failed: " <> e)
    (pure . MemoParam . Memo)
    (decodeBase64 $ encodeUtf8 t)

parseLabelParam :: Parser IndexedParam
parseLabelParam = indexedParam "label" (LabelParam . decodeText <$> takeText)

parseMessageParam :: Parser IndexedParam
parseMessageParam = indexedParam "message" (MessageParam . decodeText <$> takeText)

parseOtherParam :: Parser IndexedParam
parseOtherParam = do
  pname <- takeWhile1 paramNameChar
  idx <- option 0 (char '.' *> decimal)
  void (char '=')
  value <- decodeText <$> takeText
  pure (idx, OtherParam pname value)
  where
    paramNameChar c = isDigit c || (isAscii c && isAlpha c) || c == '+' || c == '-'

parseURI :: Text -> Either String PaymentRequest
parseURI = parseOnly zip321Parser
