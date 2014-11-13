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
import Web.Scotty

main :: IO ()
main = do 
  cfg <- parseConfig "quixotic.cfg"
  db <- openConnection $ dbName cfg
  adb <- sqliteADB db
  dbMain cfg db adb

data QConfig
  = QConfig
  { port :: Int
  , dbName :: String
  } 

parseConfig :: FilePath -> IO QConfig
parseConfig cfgFile = do 
  cfg <- C.load [C.Required cfgFile]
  QConfig <$> C.require cfg "port" <*> C.require cfg "db" 


dbMain :: QConfig -> a -> ADB IO a -> IO ()
dbMain cfg db adb = do
  scotty (port cfg) $ do
{--
Log the start time of a work interval.
Log completion of the current work interval.
Record change of a work interval start.
Record change of a work interval end.
Given a trusted token, authorize another token.
--}
    post "/logStart/:btcAddr" $ handleLogRequest db adb StartWork
    post "/logEnd/:btcAddr" $ handleLogRequest db adb StopWork

    get "/payouts" $ currentPayouts db adb

handleLogRequest :: a -> ADB IO a -> (UTCTime -> WorkEvent) -> ActionM ()
handleLogRequest db adb ev = do 
  BtcAddrParam addr <- param "btcAddr"
  timestamp <- liftIO getCurrentTime
  liftIO $ recordEvent adb db $ LogEntry addr (ev timestamp)

currentPayouts :: a -> ADB IO a -> ActionM ()
currentPayouts db adb = do 
  ptime <- liftIO getCurrentTime
  let dep = linearDepreciation (Months 6) (Months 60) 

      buildPayoutsResponse :: WorkIndex -> ActionM ()
      buildPayoutsResponse widx = json . PayoutsResponse $ payouts dep ptime widx

      payoutsAction :: EitherT T.Text ActionM WorkIndex
      payoutsAction = mapEitherT liftIO $ readWorkIndex adb db 

  eitherT (raise . LT.fromStrict) buildPayoutsResponse payoutsAction

newtype BtcAddrParam = BtcAddrParam BtcAddr 

instance Parsable BtcAddrParam where
  parseParam t = maybe (Left "Invalid BTC address") (Right . BtcAddrParam) $ (parseBtcAddr . LT.toStrict) t

newtype PayoutsResponse = PayoutsResponse Payouts

instance A.ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = A.toJSON m where
    m :: Map T.Text Double
    m = fmap fromRational (mapKeys address p)
