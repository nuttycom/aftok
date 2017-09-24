{-# LANGUAGE TemplateHaskell       #-}
module Main (main) where

import ClassyPrelude

import System.Environment            (getEnv)
import Filesystem.Path.CurrentOS (decodeString)

import qualified AftokD as D
import AftokD.AftokM (createAllPaymentRequests)

main :: IO () 
main = do
  cfgPath <- try $ getEnv "AFTOK_CFG" :: IO (Either IOError String)
  cfg <- D.loadConfig . decodeString $ either (const "conf/aftok.cfg") id cfgPath
  createAllPaymentRequests cfg
