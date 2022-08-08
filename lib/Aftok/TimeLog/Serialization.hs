{-# LANGUAGE LambdaCase #-}

module Aftok.TimeLog.Serialization
  ( depfFromJSON,
    depfToJSON,
  )
where

import Aftok.Types
import Data.Aeson
  ( Value (..),
    object,
    (.:),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Text (unpack)

depfToJSON :: DepreciationFunction -> Value
depfToJSON = \case
  LinearDepreciation undep dep ->
    object
      [ "type" .= ("LinearDepreciation" :: Text),
        "arguments" .= object ["undep" .= undep, "dep" .= dep]
      ]

depfFromJSON :: Value -> Parser DepreciationFunction
depfFromJSON = \case
  Object v -> do
    t <- v .: "type" :: Parser Text
    args <- v .: "arguments"
    case unpack t of
      "LinearDepreciation" ->
        let undep = (args .: "undep")
            dep = (args .: "dep")
         in LinearDepreciation <$> undep <*> dep
      x -> fail $ "No depreciation function recognized for type " <> x
  _ -> fail $ "Cannot interpret non-object value as a depreciation function."
