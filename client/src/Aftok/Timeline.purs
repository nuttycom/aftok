module Aftok.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (State, put, get, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Data.Array (reverse)
import Data.Date as D
import Data.Date (Date, year, month, day)
import Data.DateTime as DT
import Data.DateTime (DateTime(..), date)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime, toDateTime)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (any, length)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing, fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), Days(..))
import Data.Traversable (traverse_, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U
-- import Text.Format as F -- (format, zeroFill, width)

import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)

import Halogen as H
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource

import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import CSS (backgroundColor, clear, clearBoth, border, rgb, solid, borderRadius, marginLeft)
import CSS.Display (display, flex)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)

import Aftok.Api.Timeline as TL
import Aftok.Api.Timeline (TimelineError, Interval'(..), Interval, TimeSpan, start, end, interval)
import Aftok.Project as Project
import Aftok.Project (Project, Project'(..), ProjectId, pidStr)
import Aftok.Types (System)

type TimelineLimits =
  { bounds  :: Interval
  , current :: Instant
  }

type DayIntervals = 
  { dayBounds :: Interval
  , loggedIntervals :: Array Interval
  }

type History = M.Map Date DayIntervals

type TimelineState =
  { selectedProject :: Maybe Project
  , history :: M.Map Date DayIntervals
  , active  :: Maybe Interval
  , activeHistory :: M.Map Date DayIntervals
  }

data TimelineAction
  = Initialize
  | ProjectSelected Project.Project
  | Start
  | Stop
  | Refresh

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
      { selectedProject: Nothing
      , history: M.empty
      , active: Nothing
      , activeHistory: M.empty
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
             ] <> (historyLine <$> reverse (M.toUnfoldable $ unionHistories st.history st.activeHistory))
            )
          ]
        ]

    eval :: TimelineAction -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    eval action = do
      case action of
        Initialize -> do
          void $ H.subscribe caps.timer

        ProjectSelected p -> do
          active <- isJust <$> H.gets (_.active)
          currentProject <- H.gets (_.selectedProject)
          when (active && any (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
               (traverse_ logEnd currentProject)
          timeSpan <- TL.Before <$> lift system.nowDateTime -- FIXME, should come from a form control
          intervals' <- lift $ caps.listIntervals (unwrap p).projectId timeSpan
          intervals <- lift $ case intervals' of
                Left err -> 
                  (system.log $ "Error occurred listing intervals") *>
                  pure [] 
                Right ivals -> 
                  (system.log $ "Got " <> show (length ivals :: Int) <> " intervals for project " <> pidStr (unwrap p).projectId) *>
                  pure ivals
          history' <- lift <<< runMaybeT $ toHistory system intervals
          hist <- case history' of
              Nothing -> lift $ system.log "Project history was empty." *> pure M.empty
              Just h -> pure h
          H.modify_ (_ { selectedProject = Just p, history = hist })

        Start   -> do
          project <- H.gets (_.selectedProject)
          traverse_ logStart project

        Stop    -> do
          currentProject <- H.gets (_.selectedProject)
          traverse_ logEnd currentProject

        Refresh -> do
          t <- lift $ system.now
          H.modify_ (refresh t)

      -- common updates, irrespective of action
      active <- H.gets (_.active)
      activeHistory <- lift <<< map (fromMaybe M.empty) <<< runMaybeT $ toHistory system (U.fromMaybe active)
      H.modify_ (_ { activeHistory = activeHistory })

    logStart :: Project -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    logStart (Project' p) = do
      logged <- lift $ caps.logStart p.projectId
      case logged of
        Left err -> lift <<< system.log $ "Failed to start timer: " <> show err
        Right t -> H.modify_ (updateStart t)

    logEnd :: Project -> H.HalogenM TimelineState TimelineAction Slots output m Unit
    logEnd (Project' p) = do
      logged <- lift $ caps.logEnd p.projectId
      case logged of
        Left err -> lift <<< system.log $ "Failed to stop timer: " <> show err
        Right t -> do
          currentState <- H.get
          updatedState <- lift $ updateStop system t currentState
          H.put updatedState

historyLine 
  :: forall w i
  .  Tuple Date DayIntervals
  -> HH.HTML w i
historyLine (Tuple d xs) = 
  datedLine d xs.dayBounds xs.loggedIntervals

datedLine 
  :: forall w i
  .  Date
  -> Interval 
  -> Array Interval
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
      , P.classes (ClassName <$> ["my-2"])
      ]
      (evalState (traverse (intervalHtml dateBounds) xs) 0.0)
    ]
  where 
    px5 = px 5.0

dateStr :: Date -> String
dateStr d = (show <<< fromEnum $ year d) <> "-" 
         <> (show <<< fromEnum $ month d) <> "-" 
         <> (show <<< fromEnum $ day d)

intervalHtml
  :: forall w i 
  .  Interval
  -> Interval
  -> State Number (HH.HTML w i)
intervalHtml (Interval limits) (Interval i) = do
  offset <- get
  let maxWidth = ilen limits.start limits.end
      ileft = ilen limits.start i.start 
      iwidth = ilen i.start i.end
      px5 = px (5.0)
      toPct n = 100.0 * n / maxWidth
  put $ toPct (ilen limits.start i.end)
  pure $ HH.div
    [ CSS.style do
        backgroundColor (rgb 0xf0 0x98 0x18)
        marginLeft (pct $ toPct ileft - offset)
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


updateStart :: Instant -> TimelineState -> TimelineState
updateStart t s = 
  s { active = s.active <|> Just (TL.interval t t) }

updateStop 
  :: forall m
  .  Monad m
  => System m 
  -> Instant 
  -> TimelineState 
  -> m TimelineState
updateStop system t st = do
  newHistory <- join <$> traverse (\i -> runMaybeT $ toHistory system [TL.interval (start i) t]) st.active
  pure { selectedProject: st.selectedProject
       , history: maybe st.history (unionHistories st.history) newHistory 
       , active: Nothing
       , activeHistory: M.empty
       }

refresh :: Instant -> TimelineState -> TimelineState
refresh t s =
  s { active = map (\(Interval i) -> TL.interval i.start t) s.active
    }

ilen :: Instant -> Instant -> Number
ilen _start _end =
  let n (Milliseconds x) = x
  in  n (unInstant _end) - n (unInstant _start)

apiCapability :: Capability Aff
apiCapability = 
  { timer: timer
  , logStart: TL.apiLogStart
  , logEnd: TL.apiLogEnd 
  , listIntervals: TL.apiListIntervals
  }

mockCapability :: Capability Aff
mockCapability =
  { timer: timer
  , logStart: \_ -> Right <$> liftEffect now
  , logEnd:  \_ -> Right <$> liftEffect now
  , listIntervals: \_ _ -> Right <$> pure []
  }

utcDayBounds :: Instant -> Interval
utcDayBounds i =
  let startOfDay = DateTime (date $ toDateTime i)  bottom
      endOfDay = DT.adjust (Days 1.0) startOfDay
      startInstant = fromDateTime startOfDay
   in TL.interval startInstant (maybe startInstant fromDateTime endOfDay)

localDayBounds 
  :: forall m
  .  Monad m
  => System m
  -> Instant 
  -> MaybeT m (Tuple Date Interval)
localDayBounds system t = do
  Tuple date start <- MaybeT $ system.dateFFI.midnightLocal t
  end <- MaybeT <<< pure $ fromDateTime <$> DT.adjust (Days 1.0) (toDateTime start)
  pure $ Tuple date (interval start end)

incrementDayBounds :: Tuple Date Interval -> Maybe (Tuple Date Interval)
incrementDayBounds (Tuple d i) =
  let nextEnd = fromDateTime <$> (DT.adjust (Days 1.0) $ toDateTime (end i))
   in Tuple <$> D.adjust (Days 1.0) d
            <*> (interval (end i) <$> nextEnd)

splitInterval 
  :: forall m
  .  Monad m
  => System m
  -> Interval 
  -> MaybeT m (Array (Tuple Date DayIntervals))
splitInterval system i = do
  lift <<< system.log $ "Splitting interval " <> show i
  dayBounds@(Tuple date bounds) <- localDayBounds system (start i)
  split <- if end i < (end bounds)
     then do
       pure [Tuple date { dayBounds: bounds, loggedIntervals: [i] }]
     else do
       let firstFragment = [ Tuple date { dayBounds: bounds
                           , loggedIntervals: [interval (start i) (end bounds)] 
                           } ]
       append firstFragment <$> splitInterval system (interval (end bounds) (end i))
  lift <<< system.log $ "Split result: " <> show split
  pure split

toHistory 
  :: forall m
  .  Monad m
  => System m
  -> Array Interval 
  -> MaybeT m (M.Map Date DayIntervals)
toHistory system xs = do
  splitIntervals <- join <$> traverse (splitInterval system) xs
  pure $ M.fromFoldableWith unionDayIntervals splitIntervals

unionDayIntervals :: DayIntervals -> DayIntervals -> DayIntervals
unionDayIntervals d1 d2 = 
  { dayBounds: d1.dayBounds -- FIXME, need to be sure these match
  , loggedIntervals: d1.loggedIntervals <> d2.loggedIntervals 
  }

unionHistories :: History -> History -> History
unionHistories = M.unionWith unionDayIntervals

