module Aftok.Api.Project where

import Prelude
import Control.Monad.Except.Trans (runExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
-- import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
-- import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMapDefaultR)
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Rational (Rational, (%))
import Data.Time.Duration (Hours(..), Days(..))
import Data.Traversable (class Traversable, traverse)
-- import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (get)
import Affjax.ResponseFormat as RF
import Aftok.Types
  ( UserId
  , ProjectId
  )
import Aftok.Api.Types
  ( APIError )
import Aftok.Api.Json 
  ( decompose
  , parseDatedResponse
  )

data DepreciationFn
  = LinearDepreciation { undep :: Days, dep :: Days }

instance decodeDepreciationFn :: DecodeJson DepreciationFn where
  decodeJson json = do
    x <- decodeJson json
    dtype <- x .: "type"
    args <- x .: "arguments"
    case dtype of
      "LinearDepreciation" -> do
        undep <- Days <$> args .: "undep"
        dep <- Days <$> args .: "dep"
        pure $ LinearDepreciation { undep, dep }
      other -> Left $ "Unrecognized depreciation function: " <> other

newtype Project' date
  = Project'
  { projectId :: ProjectId
  , projectName :: String
  , inceptionDate :: date
  , initiator :: UserId
  , depFn :: DepreciationFn
  }

derive instance projectNewtype :: Newtype (Project' a) _

derive instance projectFunctor :: Functor Project'

instance projectFoldable :: Foldable Project' where
  foldr f b (Project' p) = f (p.inceptionDate) b
  foldl f b (Project' p) = f b (p.inceptionDate)
  foldMap = foldMapDefaultR

instance projectTraversable :: Traversable Project' where
  traverse f (Project' p) = Project' <<< (\b -> p { inceptionDate = b }) <$> f (p.inceptionDate)
  sequence = traverse identity

type Project
  = Project' DateTime

instance decodeJsonProject :: DecodeJson (Project' String) where
  decodeJson json = do
    x <- decodeJson json
    project <- x .: "project"
    projectId <- x .: "projectId"
    projectName <- project .: "projectName"
    inceptionDate <- project .: "inceptionDate"
    initiator <- project .: "initiator"
    depFn <- project .: "depreciationFn"
    pure $ Project' { projectId, projectName, inceptionDate, initiator, depFn }

newtype Contributor' date
  = Contributor'
  { userId :: UserId
  , handle :: String
  , joinedOn :: date
  , timeDevoted :: Hours
  , revShare :: Rational
  }

instance decodeJsonContributor :: DecodeJson (Contributor' String) where
  decodeJson json = do
    x <- decodeJson json
    userId <- x .: "userId"
    handle <- x .: "username"
    joinedOn <- x .: "joinedOn"
    timeDevoted <- Hours <$> x .: "timeDevoted"
    revShareObj <- x .: "revenueShare"
    num <- revShareObj .: "numerator"
    den <- revShareObj .: "denominator"
    let revShare = num % den
    pure $ Contributor' { userId, handle, joinedOn, timeDevoted, revShare }

newtype ProjectDetail' date
  = ProjectDetail'
  { project :: Project' date
  , contributors :: M.Map UserId (Contributor' date)
  }

type ProjectDetail = ProjectDetail' DateTime

type ProjectDetailJson date = 
  { project :: Project' date
  , contributors :: Array (Contributor' date)
  }

instance decodeJsonProjectDetail :: DecodeJson (ProjectDetail' String) where
  decodeJson json = do
    x <- decodeJson json
    project <- x .: "project"
    contributors <- x .: "contributors"
    pure $ ProjectDetail' { project, contributors }

listProjects :: Aff (Either APIError (Array Project))
listProjects = do
  response <- get RF.json "/api/projects"
  EC.liftEffect 
    <<< runExceptT
    <<< map decompose
    <<< map (map toDateTime)
    $ parseDatedResponse response

-- getProjectDetail :: ProjectId -> Aff (Maybe ProjectDetail)
-- getProjectDetail pid = do
--   response <- get RF.json ("/api/user/projects/" <> pidStr pid)
--   EC.liftEffect
--    <<< map (\dt -> ProjectDetail' { 
--              project: dt.project, 
--              contributors: M.fromFoldable $ map (\c -> (Tuple c.userId c)) dt.contributors
--            })
--    $ parsed


