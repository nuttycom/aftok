{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Bitcoin
  ( Satoshi (..),
    _Satoshi,
    ssub,
    NetworkMode (..),
    renderNetworkMode,
    parseNetworkMode,
    getNetwork,
    H.Address,
  )
where

import Bippy.Types (Satoshi (..))
import Control.Lens
import qualified Data.Configurator.Types as C
import qualified Haskoin.Address as H
import Haskoin.Constants

_Satoshi :: Iso' Satoshi Word64
_Satoshi = iso (\(Satoshi w) -> w) (Satoshi)

ssub :: Satoshi -> Satoshi -> Maybe Satoshi
ssub (Satoshi a) (Satoshi b) | a > b = Just . Satoshi $ (a - b)
ssub _ _ = Nothing

data NetworkMode
  = LiveMode
  | TestMode

renderNetworkMode :: NetworkMode -> Text
renderNetworkMode = \case
  LiveMode -> "live"
  TestMode -> "test"

parseNetworkMode :: Text -> Maybe NetworkMode
parseNetworkMode = \case
  "test" -> Just TestMode
  "live" -> Just LiveMode
  _ -> Nothing

instance C.Configured NetworkMode where
  convert (C.String t) = parseNetworkMode t
  convert _ = Nothing

getNetwork :: NetworkMode -> Network
getNetwork = \case
  LiveMode -> btc
  TestMode -> btcTest
