{-# LANGUAGE TemplateHaskell #-}

module AftokD where

import qualified Aftok.Config as AC
import Aftok.Types (Email (..))
import Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Database.PostgreSQL.Simple (ConnectInfo)
import Filesystem.Path.CurrentOS
  ( encodeString,
    fromText,
  )
import qualified Filesystem.Path.CurrentOS as P

data PaymentRequestConfig = PaymentRequestConfig
  { _aftokHost :: Text,
    _templatePath :: P.FilePath,
    _billingFromEmail :: Email
  }

makeLenses ''PaymentRequestConfig

data Config = Config
  { _smtpConfig :: AC.SmtpConfig,
    _billingConfig :: AC.BillingConfig,
    _dbConfig :: ConnectInfo,
    _paymentRequestConfig :: PaymentRequestConfig
  }

makeLenses ''Config

loadConfig :: P.FilePath -> IO Config
loadConfig cfgFile = readConfig =<< C.load [C.Required $ encodeString cfgFile]

readConfig :: CT.Config -> IO Config
readConfig cfg =
  Config
    <$> (AC.readSmtpConfig $ C.subconfig "smtp" cfg)
    <*> (AC.readBillingConfig $ C.subconfig "billing" cfg)
    <*> (AC.readConnectInfo $ C.subconfig "db" cfg)
    <*> (readPaymentRequestConfig $ C.subconfig "payment_requests" cfg)

readPaymentRequestConfig :: CT.Config -> IO PaymentRequestConfig
readPaymentRequestConfig cfg =
  PaymentRequestConfig
    <$> C.require cfg "aftok_host"
    <*> (fromText <$> C.require cfg "template_path")
    <*> (Email <$> C.require cfg "payment_from_email")
