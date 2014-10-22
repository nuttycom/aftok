module Quixotic.Database 
  ( ADB(..)
  ) where

import Control.Monad.Trans.Either
import Data.Text
import Quixotic.TimeLog

data ADB m a = ADB 
  { recordEvent :: a -> LogEntry -> m ()
  , readWorkIndex :: a -> EitherT Text m WorkIndex
  }
