module Aftok.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (withExceptT, runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (reverse, filter)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Date (Date, year, month, day)
import Data.DateTime (DateTime(..), adjust, date)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime, toDateTime)
import Data.Either (Either(..), note)
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, any, foldMapDefaultR, intercalate, foldr, foldl, length)
import Data.JSDate as JD
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), Days(..))
import Data.Traversable (class Traversable, traverse_, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (fromMaybe)
import Data.UUID as UUID
import Math (abs)
import Type.Proxy (Proxy(..))
-- import Text.Format as F -- (format, zeroFill, width)

import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)

import Affjax (get, post)
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

import CSS (backgroundColor, clear, clearBoth, border, rgb, solid, borderRadius, left)
import CSS.Display (position, absolute)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)

import Aftok.Project as Project
import Aftok.Project (Project, Project'(..), ProjectId(..), pidStr)
import Aftok.Types (APIError, System, JsonCompose, decompose, parseDatedResponse)

data Event' i
  = StartEvent i
  | StopEvent i

type Event = Event' Instant

derive instance eventFunctor :: Functor Event'

instance eventFoldable :: Foldable Event' where
  foldr f b = case _ of
    StartEvent a -> f a b
    StopEvent a -> f a b
  foldl f b = case _ of
    StartEvent a -> f b a
    StopEvent a -> f b a
  foldMap = foldMapDefaultR

instance eventTraversable :: Traversable Event' where
  traverse f = case _ of
    StartEvent a -> StartEvent <$> f a 
    StopEvent a -> StopEvent <$> f a 
  sequence = traverse identity

instance decodeJsonEvent :: DecodeJson (Event' String) where
  decodeJson json = do
    obj <- decodeJson json
    event <- obj .: "event"
    start' <- traverse (_ .: "eventTime") =<< event .:? "start"
    stop' <-  traverse (_ .: "eventTime") =<< event .:? "stop"
    note "Only 'stop' and 'start' events are supported." $ (StartEvent <$> start') <|> (StopEvent <$> stop')

newtype Interval' i = Interval
  { start :: i
  , end :: i
  }

derive instance intervalEq :: (Eq i) => Eq (Interval' i)
derive instance intervalNewtype :: Newtype (Interval' i) _

type Interval = Interval' Instant

derive instance intervalFunctor :: Functor Interval'

instance intervalFoldable :: Foldable Interval' where
  foldr f b (Interval i) = f i.start (f i.end b) 
  foldl f b (Interval i) = f (f b i.start) i.end 
  foldMap = foldMapDefaultR

instance intervalTraversable :: Traversable Interval' where
  traverse f (Interval i) = interval <$> f i.start <*> f i.end 
  sequence = traverse identity

instance decodeJsonInterval :: DecodeJson (Interval' String) where
  decodeJson json = do
    obj <- decodeJson json
    interval <$> obj .: "start" <*> obj .: "end"

interval :: forall i. i -> i -> Interval' i
interval s e = Interval { start: s, end: e }

data TimeSpan' t
  = Before t
  | During (Interval' t)
  | After t

type TimeSpan = TimeSpan' DateTime

derive instance timeSpanFunctor :: Functor TimeSpan'
instance timeSpanFoldable :: Foldable TimeSpan' where
  foldr f b = case _ of
    Before a -> f a b
    During x -> foldr f b x
    After  a -> f a b
  foldl f b = case _ of
    Before a -> f b a
    During x -> foldl f b x
    After  a -> f b a
  foldMap = foldMapDefaultR

instance timeSpanTraversable :: Traversable TimeSpan' where
  traverse f = case _ of
    Before a -> Before <$> f a
    During x -> During <$> traverse f x
    After  a -> After <$> f a
  sequence = traverse identity

type TimelineLimits =
  { bounds  :: Interval
  , current :: Instant
  }

type TimelineState =
  { limits  :: TimelineLimits
  , history :: M.Map Date (Array Interval)
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
  | Unexpected String

instance showTimelineError :: Show TimelineError where
  show = case _ of
    LogFailure e -> show e
    Unexpected t -> t

type Slot id = forall query. H.Slot query Void id

type Slots =
  ( projectList :: Project.ProjectListSlot Unit
  )

_projectList = SProxy :: SProxy "projectList"

type Capability m =
  { timer :: EventSource m TimelineAction
  , logStart :: ProjectId -> m (Either TimelineError Instant)
  , logEnd :: ProjectId -> m (Either TimelineError Instant)
  , listIntervals :: ProjectId -> TimeSpan -> m (Either TimelineError (Array Interval))
  }

component
  :: forall query input output m
  .  Monad m
  => System m
  -> Capability m
  -> Project.Capability m
  -> H.Component HH.HTML query input output m
component system caps pcaps = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = eval
      , initialize = Just Initialize
      }
  } where
    initialState :: input -> TimelineState
    initialState _ =
      { limits: { bounds: interval bottom bottom, current: bottom }
      , history: M.empty
      , active: Nothing
      , selectedProject: Nothing
      }

    render :: TimelineState -> H.ComponentHTML TimelineAction Slots m
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
            [HH.text "Your project timeline"]
          ,HH.div_
            [HH.slot _projectList unit (Project.projectListComponent system pcaps) unit (Just <<< ProjectSelected)]
          ,HH.div
            [P.classes (ClassName <$> if isNothing st.selectedProject then ["collapse"] else [])]
            ([HH.div_
              [HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-left", "my-2"])
                ,E.onClick \_ -> Just Start
                ,P.disabled (isJust st.active)
                ]
                [HH.text "Start Work"]
              ,HH.button
                [P.classes (ClassName <$> ["btn", "btn-primary", "float-right", "my-2"])
                ,E.onClick \_ -> Just Stop
                ,P.disabled (isNothing st.active)
                ]
                [HH.text "Stop Work"]
              ]
            , lineHtml $ intervalHtml st.limits.bounds <$> currentHistory st
            ] <> ((\(Tuple d xs) -> dateLine st d xs) <$> priorHistory st))
          ]
        ]

    eval :: TimelineAction -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    eval = case _ of
      Initialize -> do
        dt@(DateTime today t) <- lift system.nowDateTime
        H.put $ { limits : { bounds: dateBounds today
                           , current: fromDateTime dt
                           }
                , history : M.empty 
                , active : Nothing
                , selectedProject: Nothing
                }
        _ <- H.subscribe caps.timer
        pure unit

      ProjectSelected p -> do
        active <- isJust <$> H.gets (_.active)
        currentProject <- H.gets (_.selectedProject)
        when (active && any (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
             (traverse_ logEnd currentProject)
        timeSpan <- Before <$> lift system.nowDateTime -- FIXME, should come from a form control
        intervals' <- lift $ caps.listIntervals (unwrap p).projectId timeSpan
        let intervals = case intervals' of
             Left err -> [] -- FIXME
             Right ivals -> ivals
        H.modify_ (_ { selectedProject = Just p, history = toHistory intervals })

      Start   -> do
        project <- H.gets (_.selectedProject)
        traverse_ logStart project

      Stop    -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ logEnd currentProject

      Refresh -> do
        t <- lift $ system.now
        H.modify_ (refresh t)

    logStart :: Project -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    logStart (Project' p) = do
      logged <- lift $ caps.logStart p.projectId
      case logged of
        Left err -> lift <<< system.log $ "Failed to start timer: " <> show err
        Right t -> H.modify_ (start t)

    logEnd :: Project -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    logEnd (Project' p) = do
      logged <- lift $ caps.logEnd p.projectId
      case logged of
        Left err -> lift <<< system.log $ "Failed to stop timer: " <> show err
        Right t -> H.modify_ (stop t)

dateBounds :: Date -> Interval
dateBounds date =
  let startOfDay = DateTime date bottom
      endOfDay = adjust (Days 1.0) startOfDay
      startInstant = fromDateTime startOfDay
   in interval startInstant (maybe startInstant fromDateTime endOfDay)

currentHistory 
  :: TimelineState
  -> Array Interval
currentHistory st =
  let currentDate = date $ toDateTime st.limits.current
   in maybe [] identity (M.lookup currentDate st.history) <> fromMaybe st.active

priorHistory 
  :: TimelineState
  -> Array (Tuple Date (Array Interval))
priorHistory st = 
  let currentDate = date $ toDateTime st.limits.current
   in reverse <<< filter (not <<< (currentDate == _) <<< fst) $ M.toUnfoldable st.history

dateLine 
  :: forall action slots m
  .  TimelineState
  -> Date
  -> Array Interval
  -> H.ComponentHTML action slots m
dateLine st d xs =
  HH.div
    []
    [ HH.text $ dateStr d <> ": " <> show (length xs :: Int)
    , lineHtml (intervalHtml (dateBounds d) <$> xs)
    ]

dateStr :: Date -> String
dateStr d = (show <<< fromEnum $ year d) <> "-" 
         <> (show <<< fromEnum $ month d) <> "-" 
         <> (show <<< fromEnum $ day d)

lineHtml
  :: forall action slots m
  .  Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
lineHtml contents =
  let px5 = px 5.0
   in HH.div
    [ CSS.style do
        clear clearBoth
        border solid (px 3.0) (rgb 0x00 0xFF 0x00)
        height (px 50.0)
        borderRadius px5 px5 px5 px5
    , P.classes (ClassName <$> ["my-2"])
    ]
    contents

intervalHtml
  :: forall action slots m
  .  Interval
  -> Interval
  -> H.ComponentHTML action slots m
intervalHtml (Interval limits) (Interval i) =
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


start :: Instant -> TimelineState -> TimelineState
start t s =
  s { active = s.active <|> Just (interval t t)
    }

stop :: Instant -> TimelineState -> TimelineState
stop t s =
  s { history = maybe 
        s.history 
        (\i -> M.unionWith (<>) (toHistory [interval (unwrap i).start t]) s.history) 
        s.active
    , active = Nothing
    }

refresh :: Instant -> TimelineState -> TimelineState
refresh t s =
  s { limits = s.limits { current = t }
    , active = map (\(Interval i) -> interval i.start t) s.active
    }

ilen :: Instant -> Instant -> Number
ilen _start _end =
  let n (Milliseconds x) = x
  in  abs $ n (unInstant _end) - n (unInstant _start)

apiLogStart :: ProjectId -> Aff (Either TimelineError Instant)
apiLogStart (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logStart") requestBody
  liftEffect <<< runExceptT $ do
    event <- withExceptT LogFailure $ parseDatedResponse response
    case event of 
      StartEvent t -> pure t
      StopEvent  _ -> throwError <<< Unexpected $ "Expected start event, got stop."

apiLogEnd :: ProjectId -> Aff (Either TimelineError Instant)
apiLogEnd (ProjectId pid) = do
  let requestBody = Just <<< RB.Json <<< encodeJson $ { schemaVersion: "2.0" }
  response <- post RF.json ("/api/user/projects/" <> UUID.toString pid <> "/logEnd") requestBody
  liftEffect <<< runExceptT $ do
    event <- withExceptT LogFailure $ parseDatedResponse response
    case event of
      StartEvent _ -> throwError <<< Unexpected $ "Expected stop event, got start."
      StopEvent  t -> pure t


newtype ListIntervalsResponse a = ListIntervalsResponse
  { workIndex :: Array ({ intervals :: Array a }) 
  }

derive instance listIntervalsResponseNewtype :: Newtype (ListIntervalsResponse a) _
derive instance listIntervalsResponseFunctor :: Functor ListIntervalsResponse

instance listIntervalsResponseFoldable :: Foldable ListIntervalsResponse where
  foldr f b (ListIntervalsResponse r) = foldr f b (r.workIndex >>= _.intervals)
  foldl f b (ListIntervalsResponse r) = foldl f b (r.workIndex >>= _.intervals)
  foldMap = foldMapDefaultR

instance listIntervalsResponseTraversable :: Traversable ListIntervalsResponse where
  traverse f (ListIntervalsResponse r) = 
    let traverseCreditRow r' = ({ intervals: _ }) <$> traverse f r'.intervals
     in (ListIntervalsResponse <<< ({ workIndex: _ })) <$> traverse traverseCreditRow r.workIndex
  sequence = traverse identity

instance listIntervalsResponseDecodeJson :: DecodeJson a => DecodeJson (ListIntervalsResponse a) where
  decodeJson = map ListIntervalsResponse <<< decodeJson

_ListIntervalsResponse :: Proxy (JsonCompose ListIntervalsResponse Interval' String)
_ListIntervalsResponse = Proxy

apiListIntervals :: ProjectId -> TimeSpan -> Aff (Either TimelineError (Array Interval))
apiListIntervals pid ts = do
  ts' <- liftEffect $ traverse (JD.toISOString <<< JD.fromDateTime) ts
  let queryElements = case ts' of
        Before t -> ["before=" <> t]
        During (Interval x) -> ["after=" <> x.start, "before=" <> x.end]
        After  t -> ["after=" <> t]
  response <- get RF.json ("/api/user/projects/" <> pidStr pid <> "/workIndex?" <> intercalate "&" queryElements)
  liftEffect 
    <<< runExceptT 
    <<< map (\(ListIntervalsResponse r) -> r.workIndex >>= (_.intervals))
    <<< map decompose
    <<< withExceptT LogFailure 
      $ parseDatedResponse response

apiCapability :: Capability Aff
apiCapability = 
  { timer: timer
  , logStart: apiLogStart
  , logEnd: apiLogEnd 
  , listIntervals: apiListIntervals
  }

mockCapability :: Capability Aff
mockCapability =
  { timer: timer
  , logStart: \_ -> Right <$> liftEffect now
  , logEnd:  \_ -> Right <$> liftEffect now
  , listIntervals: \_ _ -> Right <$> pure []
  }

intervalDate :: Interval -> Date
intervalDate = date <<< toDateTime <<< (_.end) <<< unwrap

toHistory :: Array Interval -> M.Map Date (Array Interval)
toHistory = M.fromFoldableWith (<>) <<< map (\i -> Tuple (intervalDate i) [i])

