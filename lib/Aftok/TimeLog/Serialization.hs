{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Aftok.TimeLog.Serialization
  ( depfFromJSON
  , depfToJSON
  ) where

import ClassyPrelude
import Control.Applicative ((<*>))

import Data.Aeson          (Value(..), (.=), (.:), object)
import Data.Aeson.Types    (Parser)
import Data.Functor        ((<$>))

import Aftok.Types

depfToJSON :: DepreciationFunction -> Value
depfToJSON = \case
  LinearDepreciation (Months up) (Months dp) ->
    object [ "type" .= ("LinearDepreciation" :: Text)
           , "arguments" .= object [ "undep" .= up
                                   , "dep" .= dp
                                   ]
           ]

depfFromJSON :: Value -> Parser DepreciationFunction
depfFromJSON = \case
  Object v -> do
    t <- v .: "type" :: Parser Text
    args <- v .: "arguments"
    case unpack t of
      "LinearDepreciation" ->
        let undep = Months <$> (args .: "undep")
            dep   = Months <$> (args .: "dep")
        in  LinearDepreciation <$> undep <*> dep
      x -> fail $ "No depreciation function recognized for type " <> x

  _ ->
    fail $ "Cannot interpret non-object value as a depreciation function."


