module Main where

import Aftok.Server
  ( readServerConf,
    runServer,
  )
import Control.Applicative ((<**>))
import qualified Data.Configurator as C
import Options.Applicative (Parser, execParser, header, help, helper, info, long, short, strOption)

data CmdArgs = CmdArgs {cfg :: String}

args :: Parser CmdArgs
args = CmdArgs <$> strOption (long "conf" <> short 'c' <> help "Configuration file")

main :: IO ()
main = do
  opts <- execParser $ info (args <**> helper) (header "The Aftok collaboration server")
  conf <- C.load [C.Required $ cfg opts]
  serverConf <- readServerConf conf
  runServer serverConf
