module Aftok.Timeline where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)

import Data.Array (cons)
import Data.DateTime (DateTime(..), adjust)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..), Days(..))

import Math (abs)

import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now, nowDateTime)

import Halogen as H
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource

import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as P

import CSS (backgroundColor, border, rgb, solid, borderRadius, marginLeft)
import CSS.Display (position, absolute)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)

type TimelineConfig = 
  { width :: Number
  }

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
  }

data TimelineAction
  = Initialize
  | Start
  | Stop
  | Refresh

type Slot id = forall query. H.Slot query Void id

component :: forall query input output. TimelineConfig -> H.Component HH.HTML query input output Aff
component conf = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval
      , initialize = Just Initialize 
      }
  } where
    initialState :: input -> TimelineState
    initialState _ = 
      let limits = { start: bottom, current: bottom, end: bottom }
          history = []
          active = Nothing
       in { limits, history, active }

    render :: forall action slots m. TimelineState -> H.ComponentHTML action slots m
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
          ,lineHtml (intervalHtml conf st.limits <$> st.history)
          ,HH.div_
            [HH.button
              [P.classes (ClassName <$> ["btn", "btn-primary", "float-left"])]
              [HH.text "Start Work"]
            ,HH.button
              [P.classes (ClassName <$> ["btn", "btn-primary", "float-right"])]
              [HH.text "Stop Work"]
            ]
          ]
        ]

    eval :: TimelineAction -> H.HalogenM TimelineState TimelineAction () output Aff Unit
    eval = case _ of
      Initialize -> do
        dt@(DateTime date t) <- liftEffect nowDateTime
        let startOfDay = DateTime date bottom
            endOfDay = adjust (Days (toNumber 1)) startOfDay
            startInstant = fromDateTime startOfDay
            limits = 
              { start: startInstant
              , current: fromDateTime dt
              , end: maybe startInstant fromDateTime endOfDay
              }
        H.put $ { limits : limits
                , history : []
                , active : Nothing
                }

      Start   -> do
        t <- liftEffect now
        H.modify_ (start t)

      Stop    -> do
        t <- liftEffect now
        H.modify_ (stop t)

      Refresh -> do
        t <- liftEffect now
        H.modify_ (refresh t)

lineHtml 
  :: forall action slots m
  .  Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
lineHtml contents =
  let px5 = px (toNumber 5)
   in HH.div
    [ CSS.style do
        border solid (px $ toNumber 3) (rgb 0x00 0xFF 0x00)
        height (px $ toNumber 50)
        borderRadius px5 px5 px5 px5
    , P.classes (ClassName <$> ["my-2"])
    ]
    contents

intervalHtml 
  :: forall action slots m
  .  TimelineConfig 
  -> TimelineLimits 
  -> Interval 
  -> H.ComponentHTML action slots m
intervalHtml conf limits i = 
  let widthRatio = conf.width / ilen limits.start limits.end
      ileft = ilen limits.start i.start * widthRatio
      iwidth = ilen i.start i.end * widthRatio
      px5 = px (toNumber 5)
   in HH.div
    [ CSS.style do  
        position absolute
        backgroundColor (rgb 0xf0 0x98 0x18)
        height (px $ toNumber 40)
        marginLeft (pct ileft)
        width (pct iwidth)
        borderRadius px5 px5 px5 px5
    ]
    []

timer :: EventSource Aff TimelineAction
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Aff.Milliseconds 1000.0
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
  s { active = map (\a -> { start: a.start, end: t }) s.active 
    }

ilen :: Instant -> Instant -> Number
ilen _start _end = 
  let n (Milliseconds x) = x
  in  abs $ n (unInstant _end) - n (unInstant _start)

