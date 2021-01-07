module Aftok.Snaplet.Util where

import Aftok.Interval (RangeQuery (..))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8
  ( decimal,
  )
import Data.ByteString.Char8 as B
import Data.Thyme.Clock as C
import Data.Thyme.Time
import Data.Time.ISO8601
import Snap.Core

timeParam :: MonadSnap m => ByteString -> m (Maybe C.UTCTime)
timeParam k = runMaybeT $ do
  bs <- MaybeT $ getParam k
  t <- MaybeT . pure . parseISO8601 $ B.unpack bs
  pure $ toThyme t

rangeQueryParam :: MonadSnap m => m RangeQuery
rangeQueryParam = do
  endpoints <- (,) <$> timeParam "after" <*> timeParam "before"
  pure $ case endpoints of
    (Just s, Just e) -> During s e
    (Nothing, Just e) -> Before e
    (Just s, Nothing) -> After s
    (Nothing, Nothing) -> Always

decimalParam :: (Integral i, MonadSnap m) => ByteString -> m (Maybe i)
decimalParam k = runMaybeT $ do
  bs <- MaybeT $ getParam k
  MaybeT . pure . either (const Nothing) Just $ parseOnly decimal bs
