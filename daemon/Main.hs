{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import qualified AftokD as D
import AftokD.AftokM (createAllPaymentRequests)
import Control.Exception (try)
import Filesystem.Path.CurrentOS (decodeString)
import System.Environment (getEnv)
import System.IO.Error (IOError)

main :: IO ()
main = do
  cfgPath <- try @IOError $ getEnv "AFTOK_CFG"
  cfg <-
    D.loadConfig . decodeString $
      either
        (const "conf/aftok.cfg")
        id
        cfgPath
  createAllPaymentRequests cfg
