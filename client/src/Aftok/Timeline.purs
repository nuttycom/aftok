module Aftok.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except, withExceptT, runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (cons)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.DateTime (DateTime(..), adjust)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime)
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, any, foldMapDefaultR)
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), Days(..))
import Data.Traversable (class Traversable, traverse_, traverse)
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
import Aftok.Types (APIError(..), parseDate)

import Effect.Class.Console as C

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

instance showTimelineError :: Show TimelineError where
  show = case _ of
    LogFailure e -> show e

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
      HH.section
        [P.classes (ClassName <$> ["section-border", "border-primary"])]
        [HH.div
          [P.classes (ClassName <$> ["container", "pt-6"])]
          [HH.h1
            [P.classes (ClassName <$> ["mb-0", "font-weight-bold", "text-center"])]
            [HH.text "Time Tracker"]
          ,HH.p
            [P.classes (ClassName <$> ["col-md-5", "text-muted", "text-center", "mx-auto"])]
            [HH.text "Today's project timeline"]
          ,HH.div_
            [HH.slot _projectList unit (Project.projectListComponent pcaps) unit (Just <<< ProjectSelected)]
          ]
          ,HH.div
            [P.classes (ClassName <$> if isNothing st.selectedProject then ["collapse"] else [])]
            [lineHtml (intervalHtml st.limits <$> st.history <> fromMaybe st.active)
            ,HH.div_
              [HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-left"])
                ,E.onClick \_ -> Just Start
                ,P.disabled (isJust st.active)
                ]
                [HH.text "Start Work"]
              ,HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-right"])
                ,E.onClick \_ -> Just Stop
                ,P.disabled (isNothing st.active)
                ]
                [HH.text "Stop Work"]
              ]
            ]
        ]

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

      ProjectSelected p -> do
        active <- isJust <$> H.gets (_.active)
        currentProject <- H.gets (_.selectedProject)
        when (active && any (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
             (traverse_ logEnd currentProject)
        H.modify_ (_ { selectedProject = Just p, history = [] })

      Start   -> do
        project <- H.gets (_.selectedProject)
        traverse_ logStart project

      Stop    -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ logEnd currentProject

      Refresh -> do
        t <- liftEffect now
        H.modify_ (refresh t)

    logStart :: Project -> H.HalogenM TimelineState TimelineAction Slots output Aff Unit
    logStart (Project' p) = do
      logged <- lift $ caps.logStart p.projectId
      case logged of
        Left err -> C.log $ "Failed to start timer: " <> show err
        Right t -> H.modify_ (start t)

    logEnd :: Project -> H.HalogenM TimelineState TimelineAction Slots output Aff Unit
    logEnd (Project' p) = do
      logged <- lift $ caps.logEnd p.projectId
      case logged of
        Left err -> C.log $ "Failed to stop timer: " <> show err
        Right t -> H.modify_ (stop t)

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
      toPct n = 100.0 * n / maxWidth
   in HH.div
    [ CSS.style do
        position absolute
        backgroundColor (rgb 0xf0 0x98 0x18)
        height (px $ 44.0)
        left (pct $ toPct ileft)
        width (pct $ max (toPct iwidth) 0.5)
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

data Event i
  = StartEvent i
  | StopEvent i

derive instance eventFunctor :: Functor Event

instance eventFoldable :: Foldable Event where
  foldr f b = case _ of
    StartEvent a -> f a b
    StopEvent a -> f a b
  foldl f b = case _ of
    StartEvent a -> f b a
    StopEvent a -> f b a
  foldMap = foldMapDefaultR

instance eventTraversable :: Traversable Event where
  traverse f = case _ of
    StartEvent a -> StartEvent <$> f a 
    StopEvent a -> StopEvent <$> f a 
  sequence = traverse identity

instance decodeJsonEvent :: DecodeJson (Event String) where
  decodeJson json = do
    obj <- decodeJson json
    event <- obj .: "event"
    start' <- traverse (_ .: "eventTime") =<< event .:? "start"
    stop' <-  traverse (_ .: "eventTime") =<< event .:? "stop"
    note "Only 'stop' and 'start' events are supported." $ (StartEvent <$> start') <|> (StopEvent <$> stop')


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

apiLogStart :: ProjectId -> Aff (Either TimelineError Instant)
apiLogStart (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  result <- post RF.json ("/api/projects/" <> UUID.toString pid <> "/logStart") requestBody
  liftEffect <<< runExceptT $ case result of
    Left err -> throwError <<< LogFailure $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> 
        throwError $ LogFailure Forbidden
      StatusCode 200 -> 
        withExceptT (LogFailure <<< ParseFailure r.body) $ do
          event <- except $ decodeJson r.body
          timeEvent <- traverse parseDate event
          case timeEvent of 
              StartEvent t -> pure $ fromDateTime t
              StopEvent  _ -> throwError $ "Expected start event, got stop."
      other -> 
        throwError <<< LogFailure $ Error { status: Just other, message: r.statusText }

apiLogEnd :: ProjectId -> Aff (Either TimelineError Instant)
apiLogEnd (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  result <- post RF.json ("/api/projects/" <> UUID.toString pid <> "/logEnd") requestBody
  liftEffect <<< runExceptT $ case result of
    Left err -> throwError <<< LogFailure $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> 
        throwError $ LogFailure Forbidden
      StatusCode 200 -> 
        withExceptT (LogFailure <<< ParseFailure r.body) $ do
          event <- except $ decodeJson r.body
          timeEvent <- traverse parseDate event
          case timeEvent of 
              StartEvent _ -> throwError $ "Expected stop event, got start."
              StopEvent  t -> pure $ fromDateTime t
      other -> 
        throwError <<< LogFailure $ Error { status: Just other, message: r.statusText }

apiCapability :: Capability Aff
apiCapability = { logStart: apiLogStart, logEnd: apiLogEnd }

mockCapability :: Capability Aff
mockCapability =
  { logStart: \_ -> Right <$> liftEffect now
  , logEnd:  \_ -> Right <$> liftEffect now
  }
