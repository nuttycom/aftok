{-# LANGUAGE TemplateHaskell #-}

module AftokD where

import ClassyPrelude hiding (FilePath)

import Control.Lens

import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import Database.PostgreSQL.Simple    (ConnectInfo)
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)

import Aftok.Types (Email(..))
import qualified Aftok.Config as AC

data PaymentRequestConfig = PaymentRequestConfig
  { _aftokHost :: Text
  , _templatePath :: FilePath
  , _billingFromEmail :: Email
  }
makeLenses ''PaymentRequestConfig

data Config = Config
  { _smtpConfig     :: AC.SmtpConfig
  , _billingConfig  :: AC.BillingConfig
  , _dbConfig       :: ConnectInfo
  , _paymentRequestConfig :: PaymentRequestConfig
  }
makeLenses ''Config

loadConfig :: FilePath -> IO Config
loadConfig cfgFile =
  readConfig =<< C.load [C.Required $ encodeString cfgFile]

readConfig :: CT.Config -> IO Config
readConfig cfg = Config
  <$> (AC.readSmtpConfig $ C.subconfig "smtp" cfg)
  <*> (AC.readBillingConfig $ C.subconfig "billing" cfg)
  <*> (AC.readConnectInfo $ C.subconfig "db" cfg)
  <*> (readPaymentRequestConfig $ C.subconfig "payment_requests" cfg)

readPaymentRequestConfig :: CT.Config -> IO PaymentRequestConfig
readPaymentRequestConfig cfg = PaymentRequestConfig
  <$> C.require cfg "aftok_host"
  <*> (fromText <$> C.require cfg "template_path")
  <*> (Email <$> C.require cfg "payment_from_email")
