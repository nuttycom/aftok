module Aftok.Api.Project where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
-- import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
-- import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr, foldl, foldMapDefaultR)
import Data.Functor.Compose (Compose(..))
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Rational (Rational, (%))
import Data.Time.Duration (Hours(..), Days(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (get)
import Affjax.ResponseFormat as RF
import Aftok.Types
  ( UserId
  , ProjectId
  , pidStr
  )
import Aftok.Api.Types
  ( APIError )
import Aftok.Api.Json 
  ( decompose
  , parseDatedResponse
  , parseDatedResponseMay
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
  , depf :: DepreciationFn
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
    depf <- project .: "depf"
    pure $ Project' { projectId, projectName, inceptionDate, initiator, depf }

newtype Contributor' date
  = Contributor'
  { userId :: UserId
  , handle :: String
  , joinedOn :: date
  , timeDevoted :: Hours
  , revShare :: Rational
  }

derive instance contributorNewtype :: Newtype (Contributor' a) _

derive instance contributorFunctor :: Functor Contributor'

instance contributorFoldable :: Foldable Contributor' where
  foldr f b (Contributor' p) = f (p.joinedOn) b
  foldl f b (Contributor' p) = f b (p.joinedOn)
  foldMap = foldMapDefaultR

instance contributorTraversable :: Traversable Contributor' where
  traverse f (Contributor' p) = 
    Contributor' <<< (\b -> p { joinedOn = b }) <$> f (p.joinedOn)
  sequence = traverse identity

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

projectDetail :: 
  forall date. 
  Project' date -> 
  M.Map UserId (Contributor' date) -> 
  ProjectDetail' date
projectDetail project contributors = 
  ProjectDetail' { project, contributors }

type ProjectDetail = ProjectDetail' DateTime

derive instance projectDetailNewtype :: Newtype (ProjectDetail' a) _

derive instance projectDetailFunctor :: Functor ProjectDetail'

instance projectDetailFoldable :: Foldable ProjectDetail' where
  foldr f b (ProjectDetail' p) = foldr f (foldr f b (p.project)) (Compose p.contributors)
  foldl f b (ProjectDetail' p) = foldl f (foldl f b (p.project)) (Compose p.contributors)
  foldMap = foldMapDefaultR

instance projectDetailTraversable :: Traversable ProjectDetail' where
  traverse f (ProjectDetail' p) = 
    projectDetail <$> traverse f p.project 
                  <*> (map unwrap $ traverse f (Compose p.contributors))
  sequence = traverse identity

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

getProjectDetail :: ProjectId -> Aff (Either APIError (Maybe ProjectDetail))
getProjectDetail pid = do
  response <- get RF.json ("/api/user/projects/" <> pidStr pid <> "/detail")
  let parsed :: ExceptT APIError Effect (Maybe (ProjectDetail' Instant))
      parsed = parseDatedResponseMay response
  EC.liftEffect 
    <<< runExceptT
    <<< map (map (map toDateTime))
    $ parsed


