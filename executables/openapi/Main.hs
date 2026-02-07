{-# LANGUAGE TypeApplications #-}

module Main where

import Aftok.API (AftokAPI)
import Aftok.API.OpenApi ()
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.OpenApi (OpenApi)
import qualified Data.ByteString.Lazy as LBS
import Servant.OpenApi (toOpenApi)

main :: IO ()
main = do
  let spec :: OpenApi = toOpenApi (Proxy @AftokAPI)
  LBS.putStr (encodePretty spec)
