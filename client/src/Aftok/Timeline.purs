module Aftok.Timeline where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (State, put, get, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (reverse, cons)
import Data.Date (Date)
import Data.DateTime as DT
import Data.DateTime (DateTime(..), date)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime, toDateTime)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing, fromMaybe)
import Data.Time.Duration (Milliseconds(..), Hours(..), Days(..))
import Data.Traversable (traverse_, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U
import Effect.Aff as Aff
import Effect.Aff (Aff, Fiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Type.Proxy (Proxy(..))

import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Subscription as HS
import CSS (backgroundColor, clear, clearBoth, border, rgb, solid, borderRadius, marginLeft)
import CSS.Display (display, flex)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)
import Aftok.Api.Timeline as TL
import Aftok.Api.Timeline
  ( TimelineError
  , Event(..)
  , Interval(..)
  , TimeInterval
  , KeyedEvent
  , TimeSpan
  , start
  , end
  , interval
  , event
  , eventTime
  , keyedEvent
  )
import Aftok.ProjectList as ProjectList
import Aftok.Types
  ( System
  , ProjectId
  , dateStr
  )

type TimelineLimits =
  { bounds :: TimeInterval
  , current :: Instant
  }

data TimelineEvent
  = LoggedEvent (KeyedEvent Instant)
  | PhantomEvent Instant

instance showTimelineEvent :: Show TimelineEvent where
  show = case _ of
    LoggedEvent kev -> "Real event at " <> show (event kev)
    PhantomEvent i -> "Phantom at " <> show i

tlEventTime :: TimelineEvent -> Instant
tlEventTime = case _ of
  LoggedEvent kev -> eventTime <<< event $ kev
  PhantomEvent i -> i

type DayIntervals =
  { dayBounds :: TimeInterval
  , loggedIntervals :: Array (Interval TimelineEvent)
  }

type History = M.Map Date DayIntervals

type Input = Maybe ProjectId

type TimelineState h =
  { selectedProject :: Maybe ProjectId
  , history :: M.Map Date DayIntervals
  , active :: Maybe (Interval TimelineEvent)
  , activeHistory :: M.Map Date DayIntervals
  , timerHandle :: Maybe h
  }

type Timer h m =
  { start :: m (Tuple (HS.Emitter TimelineAction) h)
  , stop :: h -> m Unit
  }

data TimelineAction
  = Initialize
  | ProjectSelected (Maybe ProjectId)
  | Start
  | Stop
  | Refresh
  | Finalize

type Slot id = forall query. H.Slot query ProjectList.Output id

type Slots =
  ( projectList :: ProjectList.Slot Unit
  )

_projectList = Proxy :: Proxy "projectList"

type Capability timerHandle m =
  { timer :: Timer timerHandle m
  , logStart :: ProjectId -> m (Either TimelineError (KeyedEvent Instant))
  , logEnd :: ProjectId -> m (Either TimelineError (KeyedEvent Instant))
  , listIntervals :: ProjectId -> TimeSpan -> m (Either TimelineError (Array (Interval (KeyedEvent Instant))))
  , getLatestEvent :: ProjectId -> m (Either TimelineError (Maybe (KeyedEvent Instant)))
  }

component
  :: forall timerHandle query m
   . Monad m
  => System m
  -> Capability timerHandle m
  -> ProjectList.Capability m
  -> H.Component query Input ProjectList.Output m
component system caps pcaps =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = Just <<< ProjectSelected
              , finalize = Just Finalize
              }
    }
  where
  initialState :: Input -> TimelineState timerHandle
  initialState input =
    { selectedProject: input
    , history: M.empty
    , active: Nothing
    , activeHistory: M.empty
    , timerHandle: Nothing
    }

  render :: forall h. TimelineState h -> H.ComponentHTML TimelineAction Slots m
  render st =
    HH.section
      [ P.classes (ClassName <$> [ "section-border", "border-primary" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "container", "pt-6" ]) ]
          [ HH.h1
              [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
              [ HH.text "Time Tracker" ]
          , HH.p
              [ P.classes (ClassName <$> [ "col-md-5", "text-muted", "text-center", "mx-auto" ]) ]
              [ HH.text "Your project timeline" ]
          , HH.div_
              [ HH.slot
                  _projectList
                  unit
                  (ProjectList.component system pcaps)
                  st.selectedProject
                  (\(ProjectList.ProjectChange pid) -> ProjectSelected (Just pid))
              ]
          , HH.div
              [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
              ( [ HH.div_
                    [ HH.button
                        [ P.classes (ClassName <$> [ "btn", "btn-primary", "float-left", "my-2" ])
                        , E.onClick \_ -> Start
                        , P.disabled (isJust st.active)
                        ]
                        [ HH.text "Start Work" ]
                    , HH.button
                        [ P.classes (ClassName <$> [ "btn", "btn-primary", "float-right", "my-2" ])
                        , E.onClick \_ -> Stop
                        , P.disabled (isNothing st.active)
                        ]
                        [ HH.text "Stop Work" ]
                    ]
                ]
                  <> (historyLine <$> reverse (M.toUnfoldable $ unionHistories st.history st.activeHistory))
              )
          ]
      ]

  handleAction :: TimelineAction -> H.HalogenM (TimelineState timerHandle) TimelineAction Slots ProjectList.Output m Unit
  handleAction action = do
    case action of
      Initialize -> do
        (Tuple emitter handle) <- lift caps.timer.start
        H.modify_ (_ { timerHandle = Just handle })
        void $ H.subscribe emitter
        currentProject <- H.gets (_.selectedProject)
        traverse_ setStateForProject currentProject
      ProjectSelected pidMay -> do
        oldActive <- isJust <$> H.gets (_.active)
        currentProject <- H.gets (_.selectedProject)
        when (currentProject /= pidMay) $ do
          -- End any active intervals when switching projects.
          when oldActive $ traverse_ logEnd currentProject
          traverse_ projectSelected pidMay
      Start -> do
        project <- H.gets (_.selectedProject)
        traverse_ logStart project
      Stop -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ logEnd currentProject
      Refresh -> do
        t <- lift $ system.now
        H.modify_ (refresh t)
      Finalize -> do
        handle <- H.gets (_.timerHandle)
        lift $ traverse_ caps.timer.stop handle

    -- common updates, irrespective of action
    active <- H.gets (_.active)
    activeHistory <- lift <<< map (fromMaybe M.empty) <<< runMaybeT $ toHistory system (U.fromMaybe active)
    H.modify_ (_ { activeHistory = activeHistory })
    where
    projectSelected pid = do
      setStateForProject pid
      H.raise (ProjectList.ProjectChange pid)

  logStart :: forall h. ProjectId -> H.HalogenM (TimelineState h) TimelineAction Slots ProjectList.Output m Unit
  logStart pid = do
    logged <- lift $ caps.logStart pid
    case logged of
      Left err -> lift <<< system.log $ "Failed to start timer: " <> show err
      Right t -> H.modify_ (updateStart t)

  logEnd :: forall h. ProjectId -> H.HalogenM (TimelineState h) TimelineAction Slots ProjectList.Output m Unit
  logEnd pid = do
    logged <- lift $ caps.logEnd pid
    case logged of
      Left err -> lift <<< system.log $ "Failed to stop timer: " <> show err
      Right t -> do
        currentState <- H.get
        updatedState <- lift $ updateStop system t currentState
        H.put updatedState

  setStateForProject :: forall h. ProjectId -> H.HalogenM (TimelineState h) TimelineAction Slots ProjectList.Output m Unit
  setStateForProject pid = do
    timeSpan <- TL.Before <$> lift system.nowDateTime -- FIXME, should come from a form control
    intervals' <- lift $ caps.listIntervals pid timeSpan
    intervals <-
      lift
        $ case intervals' of
            Left err ->
              (system.log $ "Error occurred listing intervals" <> show err)
                *> pure []
            Right ivals -> pure $ map (map LoggedEvent) ivals
    history' <- lift <<< runMaybeT $ toHistory system intervals
    hist <- case history' of
      Nothing -> lift $ system.log "Project history was empty." *> pure M.empty
      Just h -> pure h
    latestEventResponse <- lift $ caps.getLatestEvent pid
    -- now <- lift $ system.now
    active <-
      lift
        $ case latestEventResponse of
            Left err ->
              (system.log $ "Error occurred retrieving the latest event: " <> show err)
                *> pure Nothing
            Right latestEvent -> do
              let
                activeInterval :: TL.KeyedEvent Instant -> m (Maybe (Interval TimelineEvent))
                activeInterval ev = case event ev of
                  TL.StartEvent i ->
                    (system.log $ "Project has an open active interval starting " <> show i)
                      *> (Just <<< interval (LoggedEvent ev) <<< PhantomEvent <$> system.now)
                  TL.StopEvent _ -> pure Nothing
              join <$> traverse activeInterval latestEvent
    H.modify_ (_ { selectedProject = Just pid, history = hist, active = active })

historyLine
  :: forall w i
   . Tuple Date DayIntervals
  -> HH.HTML w i
historyLine (Tuple d xs) = datedLine d xs.dayBounds xs.loggedIntervals

datedLine
  :: forall w i
   . Date
  -> TimeInterval
  -> Array (Interval TimelineEvent)
  -> HH.HTML w i
datedLine d dateBounds xs =
  HH.div
    [ CSS.style do
        clear clearBoth
    ]
    [ HH.text $ dateStr d <> ": " <> show (length xs :: Int)
    , HH.div
        [ CSS.style do
            border solid (px 3.0) (rgb 0x00 0xFF 0x00)
            borderRadius px5 px5 px5 px5
            height (px $ 44.0)
            display flex
        , P.classes (ClassName <$> [ "my-2" ])
        ]
        (evalState (traverse (intervalHtml dateBounds) xs) 0.0)
    ]
  where
  px5 = px 5.0

intervalHtml
  :: forall w i
   . TimeInterval
  -> Interval TimelineEvent
  -> State Number (HH.HTML w i)
intervalHtml (Interval limits) (Interval i) = do
  offset <- get
  let
    maxWidth = ilen limits.start limits.end

    ileft = ilen limits.start (tlEventTime i.start)

    iwidth = ilen (tlEventTime i.start) (tlEventTime i.end)

    px5 = px (5.0)

    toPct n = 100.0 * n / maxWidth
  put $ toPct (ilen limits.start (tlEventTime i.end))
  pure
    $ HH.div
        [ CSS.style do
            backgroundColor (rgb 0xf0 0x98 0x18)
            marginLeft (pct $ toPct ileft - offset)
            width (pct $ max (toPct iwidth) 0.5)
            borderRadius px5 px5 px5 px5
        ]
        []

timer :: Timer (Fiber Unit) Aff
timer =
  { start: do
      { emitter, listener } <- H.liftEffect HS.create
      fiber <-
        Aff.forkAff
          $ forever do
              Aff.delay $ Aff.Milliseconds 10000.0
              H.liftEffect $ HS.notify listener Refresh
      pure $ Tuple emitter fiber
  , stop: Aff.killFiber (error "Event source finalized")
  }

updateStart :: forall h. KeyedEvent Instant -> TimelineState h -> TimelineState h
updateStart ev s = s { active = s.active <|> Just (TL.interval (LoggedEvent ev) (PhantomEvent <<< eventTime <<< event $ ev)) }

updateStop
  :: forall h m
   . Monad m
  => System m
  -> KeyedEvent Instant
  -> TimelineState h
  -> m (TimelineState h)
updateStop system ev st = do
  let
    updateHistory i = runMaybeT $ toHistory system [ TL.interval (start i) (LoggedEvent ev) ]
  newHistory <- join <$> traverse updateHistory st.active
  pure
    { selectedProject: st.selectedProject
    , history: maybe st.history (unionHistories st.history) newHistory
    , active: Nothing
    , activeHistory: M.empty
    , timerHandle: st.timerHandle
    }

refresh :: forall h. Instant -> TimelineState h -> TimelineState h
refresh t s =
  s
    { active = map (\i -> TL.interval (start i) (PhantomEvent t)) s.active
    }

ilen :: Instant -> Instant -> Number
ilen _start _end =
  let
    n (Milliseconds x) = x
  in
    n (unInstant _end) - n (unInstant _start)

apiCapability :: Capability (Fiber Unit) Aff
apiCapability =
  { timer: timer
  , logStart: TL.apiLogStart
  , logEnd: TL.apiLogEnd
  , listIntervals: TL.apiListIntervals
  , getLatestEvent: TL.apiLatestEvent
  }

mockCapability :: Capability (Fiber Unit) Aff
mockCapability =
  { timer: timer
  , logStart: \_ -> Right <<< keyedEvent "" <<< StartEvent <$> liftEffect now
  , logEnd: \_ -> Right <<< keyedEvent "" <<< StopEvent <$> liftEffect now
  , listIntervals: \_ _ -> Right <$> pure []
  , getLatestEvent: \_ -> Right <$> pure Nothing
  }

utcDayBounds :: Instant -> TimeInterval
utcDayBounds i =
  let
    startOfDay = DateTime (date $ toDateTime i) bottom

    endOfDay = DT.adjust (Days 1.0) startOfDay

    startInstant = fromDateTime startOfDay
  in
    TL.interval startInstant (maybe startInstant fromDateTime endOfDay)

localDayBounds
  :: forall m
   . Monad m
  => System m
  -> Instant
  -> MaybeT m (Tuple Date TimeInterval)
localDayBounds system t = do
  Tuple date start <- MaybeT $ system.dateFFI.midnightLocal t
  nextNoon <-
    MaybeT <<< pure
      $ fromDateTime
          <$>
            ( DT.adjust (Hours 12.0) <=< DT.adjust (Days 1.0)
                $ (toDateTime start)
            )
  Tuple _ end <- MaybeT $ system.dateFFI.midnightLocal nextNoon
  pure $ Tuple date (interval start end)

splitInterval
  :: forall m
   . Monad m
  => System m
  -> Interval TimelineEvent
  -> MaybeT m (Array (Tuple Date DayIntervals))
splitInterval system i = do
  lift <<< system.log $ "Splitting interval " <> show i
  -- day bounds are based on the start event.
  Tuple date bounds <- localDayBounds system (tlEventTime $ start i)
  lift <<< system.log $ "Splitting on day bounds: " <> show (start bounds) <> " to " <> show (end bounds)
  split <-
    if tlEventTime (end i) < end bounds then do
      lift <<< system.log $ "Split complete"
      pure [ Tuple date { dayBounds: bounds, loggedIntervals: [ i ] } ]
    else do
      let
        splitEvent = PhantomEvent (end bounds)

        currInterval = Tuple date { dayBounds: bounds, loggedIntervals: [ interval (start i) splitEvent ] }

        nextInterval = interval splitEvent (end i)
      lift <<< system.log $ "Split required; first fragment: " <> show currInterval <> "; next interval: " <> show nextInterval
      cons currInterval <$> splitInterval system nextInterval
  --lift <<< system.log $ "Split result: " <> show split
  pure split

toHistory
  :: forall m
   . Monad m
  => System m
  -> Array (Interval TimelineEvent)
  -> MaybeT m (M.Map Date DayIntervals)
toHistory system xs = do
  splits <- join <$> traverse (splitInterval system) xs
  pure $ M.fromFoldableWith unionDayIntervals splits

unionDayIntervals :: DayIntervals -> DayIntervals -> DayIntervals
unionDayIntervals d1 d2 =
  { dayBounds: d1.dayBounds -- FIXME, need to be sure these match
  , loggedIntervals: d1.loggedIntervals <> d2.loggedIntervals
  }

unionHistories :: History -> History -> History
unionHistories = M.unionWith unionDayIntervals
