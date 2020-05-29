{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Aftok.Currency.Bitcoin where


import qualified Data.Configurator.Types as C
import           Control.Lens
import           Network.Bippy.Types (Satoshi (..))
import           Network.Haskoin.Constants

satoshi :: Lens' Satoshi Word64
satoshi inj (Satoshi value) = Satoshi <$> inj value

ssub :: Satoshi -> Satoshi -> Maybe Satoshi
ssub (Satoshi a) (Satoshi b) | a > b = Just . Satoshi $ (a - b)
ssub _ _ = Nothing

data NetworkId
  = BTC
  | BCH
  deriving (Eq, Show, Ord)

renderNetworkId :: NetworkId -> Text
renderNetworkId = \case
  BTC -> "btc"
  BCH -> "bch"

parseNetworkId :: Text -> Maybe NetworkId
parseNetworkId = \case
  "btc" -> Just BTC
  "bch" -> Just BCH
  _ -> Nothing

data NetworkMode
  = LiveMode
  | TestMode

parseNetworkMode :: Text -> Maybe NetworkMode
parseNetworkMode = \case
  "test" -> Just TestMode
  "live" -> Just LiveMode
  _ -> Nothing

instance C.Configured NetworkMode where
  convert (C.String t) = parseNetworkMode t
  convert _ = Nothing

toNetwork :: NetworkMode -> NetworkId -> Network
toNetwork LiveMode = \case
  BTC -> btc
  BCH -> bch
toNetwork TestMode = \case
  BTC -> btcTest
  BCH -> bchTest

toNetworkId :: Network -> Maybe NetworkId
toNetworkId n = case getNetworkName n of
  "btc" -> Just BTC
  "btcTest" -> Just BTC
  "bch" -> Just BCH
  "bchTest" -> Just BCH
  _ -> Nothing
