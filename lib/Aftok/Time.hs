module Aftok.Time where

import qualified Data.Thyme.Clock as C

data Clock m = Clock
  {getCurrentTime :: m C.UTCTime}

hoistClock :: (forall a. m a -> n a) -> Clock m -> Clock n
hoistClock nt c = Clock {getCurrentTime = nt $ getCurrentTime c}

systemClock :: Clock IO
systemClock = Clock {getCurrentTime = C.getCurrentTime}
