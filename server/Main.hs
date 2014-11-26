{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import qualified Data.Aeson as A
import qualified Data.Configurator as C
import Data.Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Database.SQLite
import Quixotic
import Quixotic.Database
import Quixotic.Database.SQLite
import Quixotic.TimeLog
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

main :: IO ()
main = do
  cfg <- parseConfig "quixotic.cfg"
  db <- openConnection $ dbName cfg
  adb <- sqliteADB db
  quickHttpServe $ site cfg

site :: QConfig -> a -> ADB IO a -> Snap ()
site cfg db adb =
    route [ ("logStart/:btcAddr", handleLogRequest db adb StartWork)
          , ("logEnd/:btcAddr", handleLogRequest db adb StopWork)
          , ("payouts", currentPayouts db adb)
          ] 

data QConfig = QConfig
  { port :: Int
  , dbName :: String
  } 

parseConfig :: FilePath -> IO QConfig
parseConfig cfgFile = do 
  cfg <- C.load [C.Required cfgFile]
  QConfig <$> C.require cfg "port" <*> C.require cfg "db" 

handleLogRequest :: a -> ADB IO a -> (UTCTime -> WorkEvent) -> Snap ()
handleLogRequest db adb ev = do 
  addrBytes <- getParam "btcAddr"
  let addr = fmap T.pack addrBytes >>= parseBtcAddr 
  timestamp <- liftIO getCurrentTime
  liftIO $ recordEvent adb db $ LogEntry addr (ev timestamp)

currentPayouts :: a -> ADB IO a -> Snap ()
currentPayouts db adb = do 
  ptime <- liftIO getCurrentTime
  let dep = linearDepreciation (Months 6) (Months 60) 

      buildPayoutsResponse :: WorkIndex -> Snap ()
      buildPayoutsResponse widx = writeBS . encode . PayoutsResponse $ payouts dep ptime widx

      payoutsAction :: EitherT T.Text Snap WorkIndex
      payoutsAction = mapEitherT liftIO $ readWorkIndex adb db 

  eitherT (raise . LT.fromStrict) buildPayoutsResponse payoutsAction

newtype PayoutsResponse = PayoutsResponse Payouts

instance A.ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = A.toJSON m where
    m :: Map T.Text Double
    m = fmap fromRational (mapKeys address p)
