module Aftok.Snaplet.Util where



import           Data.ByteString.Char8     as B
import           Data.Thyme.Clock          as C
import           Data.Thyme.Time
import           Data.Time.ISO8601

import           Snap.Core

timeParam :: MonadSnap m => ByteString -> m (Maybe C.UTCTime)
timeParam k = runMaybeT $ do
  bs <- MaybeT $ getParam k
  t <- MaybeT . pure . parseISO8601 $ B.unpack bs
  pure $ toThyme t

