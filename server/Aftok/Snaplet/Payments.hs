{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.Payments
  ( listPayableRequestsHandler,
    getBip70PaymentRequestHandler,
    bip70PaymentResponseHandler,
  )
where

import Aftok.Billing (SubscriptionId (..))
import qualified Aftok.Config as AC
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import Aftok.Database
import Aftok.Payments
import Aftok.Payments.Types
  ( NativePayment (..),
    Payment' (..),
    PaymentId,
    nativeRequest,
  )
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Util (fromMaybeT)
import Control.Lens
  ( view,
    (^.),
  )
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.ProtocolBuffers (decodeMessage)
import Data.Serialize.Get (runGetLazy)
import Data.Thyme.Clock as C
-- import Network.HTTP.Client
--   ( defaultManagerSettings,
--     managerResponseTimeout,
--     responseTimeoutMicro,
--   )
-- import Network.HTTP.Client.OpenSSL
-- import Network.Wreq
--   ( defaults,
--     manager,
--   )
-- import OpenSSL.Session (context)
import Snap.Core
  ( readRequestBody,
  )
import Snap.Snaplet as S

listPayableRequestsHandler :: S.Handler App App [(PaymentRequestId, SomePaymentRequestDetail)]
listPayableRequestsHandler = do
  uid <- requireUserId
  sid <- requireId "subscriptionId" SubscriptionId
  snapEval $ findPayableRequests uid sid

bip70PaymentResponseHandler :: AC.BitcoinConfig -> S.Handler App App PaymentId
bip70PaymentResponseHandler _ = do
  requestBody <- readRequestBody 4096
  (prid, preq) <- getBip70PaymentRequestHandler
  pmnt <-
    either
      (\msg -> snapError 400 $ "Could not decode payment response: " <> show msg)
      (pure . Bitcoin.Payment Nothing Nothing Nothing (preq ^. Bitcoin.paymentRequestKey))
      (runGetLazy decodeMessage requestBody)
  now <- liftIO $ C.getCurrentTime
  -- let opts =
  --       defaults
  --         & manager
  --         .~ Left (opensslManagerSettings context)
  --         & manager
  --         .~ Left
  --           ( defaultManagerSettings
  --               { managerResponseTimeout = responseTimeoutMicro 10000
  --               }
  --           )
  -- exchResp <-
  --   liftIO
  --     . try @HttpException
  --     $ asValue
  --       =<< (withOpenSSL $ getWith opts (cfg ^. exchangeRateServiceURI))
  -- _ <- traverse (logError . T.encodeUtf8 . show) (preview _Left exchResp)
  -- (preview (_Right . responseBody) exchResp)
  let newPayment = Payment (Const prid) now (BitcoinPayment pmnt)
  snapEval . liftdb $ CreatePayment newPayment

getBip70PaymentRequestHandler :: S.Handler App App (PaymentRequestId, Bitcoin.PaymentRequest)
getBip70PaymentRequestHandler = do
  (rid, SomePaymentRequest preq) <- getBip70PaymentRequestHandler'
  case (preq ^. nativeRequest) of
    Bip70Request bp -> pure (rid, bp)
    _ -> snapError 400 $ "Not a BIP-70 bitcoin payment request."

getBip70PaymentRequestHandler' ::
  S.Handler App App (PaymentRequestId, SomePaymentRequestDetail)
getBip70PaymentRequestHandler' = do
  pkey <- Bitcoin.PaymentKey . decodeUtf8 <$> requireParam "paymentRequestKey"
  fromMaybeT
    ( snapError 404 $
        "Outstanding payment request not found for key "
          <> (view Bitcoin._PaymentKey pkey)
    )
    (mapMaybeT snapEval $ findPaymentRequestByKey pkey)
