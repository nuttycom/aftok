module Aftok.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (cons)
import Data.DateTime (DateTime(..), adjust)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), Days(..))
import Data.Traversable (traverse_)
import Data.Unfoldable (fromMaybe)
import Data.UUID as UUID

import Math (abs)

import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now, nowDateTime)

import Affjax (post, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

import Data.Argonaut.Encode (encodeJson)

import Halogen as H
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource

import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import CSS (backgroundColor, border, rgb, solid, borderRadius, left)
import CSS.Display (position, absolute)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)

import Aftok.Project as Project
import Aftok.Project (Project, Project'(..), ProjectId(..))
import Aftok.Types (APIError(..))

import Effect.Class.Console (log)

type Interval = 
  { start :: Instant
  , end :: Instant
  }

type TimelineLimits = 
  { start   :: Instant
  , current :: Instant
  , end     :: Instant
  }

type TimelineState = 
  { limits  :: TimelineLimits
  , history :: Array Interval
  , active  :: Maybe Interval
  , selectedProject :: Maybe Project
  }

data TimelineAction
  = Initialize
  | ProjectSelected Project.Project
  | Start
  | Stop
  | Refresh

data TimelineError
  = LogFailure (APIError)

type Slot id = forall query. H.Slot query Void id

type Slots = 
  ( projectList :: Project.ProjectListSlot Unit
  )

_projectList = SProxy :: SProxy "projectList"

type Capability m =
  { logStart :: ProjectId -> m (Either TimelineError Instant)
  , logEnd :: ProjectId -> m (Either TimelineError Instant)
  }

component 
  :: forall query input output
  .  Capability Aff 
  -> Project.Capability Aff
  -> H.Component HH.HTML query input output Aff
component caps pcaps = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval
      , initialize = Just Initialize 
      }
  } where
    initialState :: input -> TimelineState
    initialState _ = 
      { limits: { start: bottom, current: bottom, end: bottom }
      , history: []
      , active: Nothing
      , selectedProject: Nothing
      }

    render :: TimelineState -> H.ComponentHTML TimelineAction Slots Aff
    render st = 
      let lineForm = 
            [lineHtml (intervalHtml st.limits <$> st.history <> fromMaybe st.active)
            ,HH.div_
              [HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-left"])
                ,E.onClick \_ -> Just Start
                ]
                [HH.text "Start Work"]
              ,HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-right"])
                ,E.onClick \_ -> Just Stop
                ]
                [HH.text "Stop Work"]
              ]
            ]
       in HH.section 
          [P.classes (ClassName <$> ["section-border", "border-primary"])]
          ([HH.div
            [P.classes (ClassName <$> ["container-fluid", "pt-6"])]
            [HH.h1 
              [P.classes (ClassName <$> ["mb-0", "font-weight-bold", "text-center"])]
              [HH.text "Time Tracker"]
            ,HH.p
              [P.classes (ClassName <$> ["col-md-5", "text-muted", "text-center", "mx-auto"])]
              [HH.text "Today's project timeline"]
            ,HH.div_ 
              [HH.slot _projectList unit (Project.projectListComponent pcaps) unit (Just <<< ProjectSelected)]
            ]
          ] <> (if isJust st.selectedProject then lineForm else []))

    eval :: TimelineAction -> H.HalogenM TimelineState TimelineAction Slots output Aff Unit
    eval = case _ of
      Initialize -> do
        dt@(DateTime date t) <- liftEffect nowDateTime
        let startOfDay = DateTime date bottom
            endOfDay = adjust (Days 1.0) startOfDay
            startInstant = fromDateTime startOfDay
            limits = 
              { start: startInstant
              , current: fromDateTime dt
              , end: maybe startInstant fromDateTime endOfDay
              }
            llen = ilen limits.start limits.end
            clen = ilen limits.start limits.current
        H.put $ { limits : limits
                , history : []
                , active : Nothing
                , selectedProject: Nothing
                }
        _ <- H.subscribe timer
        pure unit

      ProjectSelected p -> 
        H.modify_ (_ { selectedProject = Just p })

      Start   -> do
        let withProject (Project' p) = do
              logged <- lift $ caps.logStart p.projectId
              case logged of 
                Left _ -> log "Failed to start timer."
                Right t -> H.modify_ (start t)
        project <- H.gets (_.selectedProject)
        log $ "Project selected? " <> show (isJust project)
        traverse_ withProject project

      Stop    -> do
        let withProject (Project' p) = do
              logged <- lift $ caps.logEnd p.projectId
              case logged of 
                Left _ -> log "Failed to stop timer."
                Right t -> H.modify_ (stop t)
        project <- H.gets (_.selectedProject)
        traverse_ withProject project

      Refresh -> do
        t <- liftEffect now
        H.modify_ (refresh t)

lineHtml 
  :: forall action slots m
  .  Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
lineHtml contents =
  let px5 = px 5.0
   in HH.div
    [ CSS.style do
        border solid (px 3.0) (rgb 0x00 0xFF 0x00)
        height (px 50.0)
        borderRadius px5 px5 px5 px5
    , P.classes (ClassName <$> ["my-2"])
    ]
    contents

intervalHtml 
  :: forall action slots m
  .  TimelineLimits 
  -> Interval 
  -> H.ComponentHTML action slots m
intervalHtml limits i = 
  let maxWidth = ilen limits.start limits.end
      ileft = ilen limits.start i.start 
      iwidth = ilen i.start i.end 
      px5 = px (5.0)
      toPct n = pct (100.0 * n / maxWidth)
   in HH.div
    [ CSS.style do  
        position absolute
        backgroundColor (rgb 0xf0 0x98 0x18)
        height (px $ 44.0)
        left (toPct ileft)
        width (toPct iwidth)
        borderRadius px5 px5 px5 px5
    ]
    []

timer :: EventSource Aff TimelineAction
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Aff.Milliseconds 10000.0
    EventSource.emit emitter Refresh

  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber


start :: Instant -> TimelineState -> TimelineState
start t s = 
  s { active = s.active <|> Just { start: t, end: t }
    }

stop :: Instant -> TimelineState -> TimelineState
stop t s = 
  s { history = maybe s.history (\st -> cons { start: st.start, end: t } s.history) s.active
    , active = Nothing
    }

refresh :: Instant -> TimelineState -> TimelineState
refresh t s = 
  s { limits = s.limits { current = t }
    , active = map (_ { end = t }) s.active 
    }

ilen :: Instant -> Instant -> Number
ilen _start _end = 
  let n (Milliseconds x) = x
  in  abs $ n (unInstant _end) - n (unInstant _start)

logStart :: ProjectId -> Aff (Either TimelineError Instant)
logStart (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  result <- post RF.json ("/api/projects/" <> UUID.toString pid <> "/logStart") requestBody
  case result of
    Left err -> pure <<< Left <<< LogFailure $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> pure <<< Left <<< LogFailure $ Forbidden
      StatusCode 200 -> Right <$> liftEffect now
      other -> pure <<< Left <<< LogFailure $ Error { status: Just other, message: r.statusText }

logEnd :: ProjectId -> Aff (Either TimelineError Instant)
logEnd (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  result <- post RF.json ("/api/projects/" <> UUID.toString pid <> "/logEnd") requestBody
  case result of
    Left err -> pure <<< Left <<< LogFailure $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> pure <<< Left <<< LogFailure $ Forbidden
      StatusCode 200 -> Right <$> liftEffect now
      other -> pure <<< Left <<< LogFailure $ Error { status: Just other, message: r.statusText }

apiCapability :: Capability Aff
apiCapability = { logStart, logEnd }

mockCapability :: Capability Aff
mockCapability = 
  { logStart: \_ -> Right <$> liftEffect now
  , logEnd:  \_ -> Right <$> liftEffect now
  }
