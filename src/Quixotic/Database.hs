module Quixotic.Database 
  ( ADB(..)
  ) where

import Control.Monad.Trans.Either
import Quixotic.TimeLog

data ADB m a = ADB 
  { recordEvent :: a -> LogEntry -> m ()
  , readWorkIndex :: a -> EitherT String m WorkIndex
  }
