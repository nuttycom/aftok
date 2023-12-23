module Aftok.Database.Events where

import Aftok.TimeLog (EventId, LogEntry)
import Aftok.Types (ProjectId, UserId)

data EventCaps (m :: Type -> Type) = EventCaps
  { createEvent :: ProjectId -> UserId -> LogEntry -> m EventId
  }

hoistEventCaps :: (forall a. m a -> n a) -> EventCaps m -> EventCaps n
hoistEventCaps nt c =
  EventCaps
    { createEvent = ((nt .) .) . createEvent c
    }
