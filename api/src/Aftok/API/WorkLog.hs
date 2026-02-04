{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | WorkLog API types for the Aftok API.
module Aftok.API.WorkLog
  ( -- * API Types
    WorkLogAPI,

    -- * Request Types
    LogStartRequest (..),
    LogEndRequest (..),
    EventAmendmentRequest (..),
    EventAmendmentType (..),
  )
where

import Aftok.Json (parseCreditToV2)
import Aftok.Types (CreditTo (..), ProjectId)
import Data.Aeson
  ( FromJSON (..),
    Value (Object),
    (.:),
    (.:?),
  )
import qualified Data.Text as T
import qualified Data.Thyme.Clock as C
import Servant.API

-- | WorkLog API for user-specific project operations
type WorkLogAPI =
  "user"
    :> "projects"
    :> Capture "projectId" ProjectId
    :> ( -- POST /user/projects/:projectId/logStart
         "logStart" :> ReqBody '[JSON] LogStartRequest :> Post '[JSON] Value
           -- POST /user/projects/:projectId/logEnd
           :<|> "logEnd" :> ReqBody '[JSON] LogEndRequest :> Post '[JSON] Value
           -- GET /user/projects/:projectId/events
           :<|> "events"
             :> QueryParam "after" C.UTCTime
             :> QueryParam "before" C.UTCTime
             :> QueryParam "limit" Int
             :> Get '[JSON] Value
           -- GET /user/projects/:projectId/workIndex
           :<|> "workIndex" :> Get '[JSON] Value
       )
    -- Event amendment
    :<|> "events"
      :> Capture "eventId" Text
      :> "amend"
      :> ReqBody '[JSON] EventAmendmentRequest
      :> Put '[JSON] Value

-- | Log start request
data LogStartRequest = LogStartRequest
  { lsrCreditTo :: Maybe CreditTo,
    lsrEventMeta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON LogStartRequest where
  parseJSON (Object o) = do
    creditTo' <- o .:? "creditTo" >>= maybe (pure Nothing) (fmap Just . parseCreditToV2)
    eventMeta' <- o .:? "eventMeta"
    pure $ LogStartRequest creditTo' eventMeta'
  parseJSON _ = mzero

-- | Log end request
data LogEndRequest = LogEndRequest
  { lerCreditTo :: Maybe CreditTo,
    lerEventMeta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON LogEndRequest where
  parseJSON (Object o) = do
    creditTo' <- o .:? "creditTo" >>= maybe (pure Nothing) (fmap Just . parseCreditToV2)
    eventMeta' <- o .:? "eventMeta"
    pure $ LogEndRequest creditTo' eventMeta'
  parseJSON _ = mzero

-- | Event amendment request
data EventAmendmentRequest = EventAmendmentRequest
  { earAmendment :: EventAmendmentType
  }
  deriving (Generic)

-- | Event amendment type
data EventAmendmentType
  = TimeChangeReq C.UTCTime
  | CreditToChangeReq CreditTo
  | MetadataChangeReq Value

instance FromJSON EventAmendmentRequest where
  parseJSON (Object o) = do
    amendType <- o .: "amendment"
    amendment <- case (amendType :: Text) of
      "timeChange" -> TimeChangeReq <$> o .: "eventTime"
      "creditToChange" -> CreditToChangeReq <$> parseCreditToV2 o
      "metadataChange" -> MetadataChangeReq <$> o .: "eventMeta"
      other -> fail $ "Amendment type " <> T.unpack other <> " not recognized."
    pure $ EventAmendmentRequest amendment
  parseJSON val = fail $ "Value " <> show val <> " is not a JSON object."
