module Quixotic.Database 
  ( ADB(..)
  ) where

import Control.Monad.Trans.Either
import Quixotic.TimeLog
import qualified Data.Text as T

data ADB m a = ADB 
  { recordEvent :: a -> LogEntry -> m ()
  , readWorkIndex :: a -> EitherT T.Text m WorkIndex
  }
