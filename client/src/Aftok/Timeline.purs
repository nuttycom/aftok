module Aftok.Timeline where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime, now)
import Control.Monad.Aff (Aff())
import Control.Alt ((<|>))

import Data.Array (cons)
--import Data.Bounded (bottom)
--import Data.Date (Date(..), day, month, year)
import Data.DateTime (DateTime(..), date, adjust)
import Data.DateTime.Instant (Instant, unInstant, fromDateTime)
import Data.DateTime.Locale (LocalValue(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..), Days(..))
import Data.Maybe (Maybe(..), maybe)

import Math (abs)

import CSS.Display (position, absolute)
import CSS.Geometry (left) --, width, top, height)
import CSS.Size (px)

import Halogen as H
import Halogen.Aff (HalogenEffects)
import Halogen.HTML.CSS as CSS
import Halogen.HTML as HH
-- import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

type Interval = 
  { start :: Instant
  , end :: Instant
  }

type TimelineLimits = 
  { start :: Instant
  , current :: Instant
  , end :: Instant
  }

type TimelineConfig = 
  { width :: Number
  }

type TimelineState = 
  { limits :: TimelineLimits
  , history :: Array Interval
  , active :: Maybe Interval
  }

data TimelineAction a
  = Start a
  | Stop a
  | Refresh a
  -- | Open a
  -- | Close a

type TimelineEffects eff = HalogenEffects (now :: NOW | eff)

initialState :: forall eff. Eff (now :: NOW | eff) TimelineState
initialState = do
  LocalValue l t <- nowDateTime
  let startOfDay = DateTime (date t) bottom
      endOfDay = adjust (Days (toNumber 1)) startOfDay
      startInstant = fromDateTime startOfDay
      limits = 
        { start: startInstant
        , current: fromDateTime t
        , end: maybe startInstant fromDateTime endOfDay
        }
  pure $ { limits : limits
         , history : []
         , active : Nothing
         }

ui :: forall eff. TimelineConfig -> TimelineState -> H.Component HH.HTML TimelineAction TimelineState Void (Aff (TimelineEffects eff))
ui conf s = H.component 
  { initialState: const s
  , render 
  , eval
  , receiver: const Nothing
  } where

  intervalHtml :: forall f. TimelineLimits -> Interval -> H.ComponentHTML f
  intervalHtml limits i = 
    let offset = ((ilen limits.start i.start) / (ilen limits.start limits.end)) * conf.width
    in  HH.div
      [ CSS.style do  
          position absolute
          left (px offset)
      ]
      [ HH.div
        [ P.classes (H.ClassName <$> ["center"]) ]
        []
      ]

      -- <div style="position: absolute; left: 582.268px; width: 92.4619px; top: 6px; height: 33px;" class="TimelineElement Element ui-resizable ui-draggable" data-original-title=""><div class="center" style="background-color: rgb(255, 0, 0);"></div><div class="ui-resizable-w ui-resizable-handle"></div><div class="ui-draggable-pad"></div><div class="ui-resizable-e ui-resizable-handle"></div></div>

  render :: TimelineState -> H.ComponentHTML TimelineAction
  render st = 
    HH.div
      [ P.classes (H.ClassName <$> ["container"]) ]
      [ HH.div
        [ P.classes (H.ClassName <$> ["tl-wrapper"]) ]
        (intervalHtml st.limits <$> st.history)
      ] 

  eval :: TimelineAction ~> H.ComponentDSL TimelineState TimelineAction Void (Aff (TimelineEffects eff))
  -- The user has requested to start the clock.
  eval (Start next) = do 
    t <- liftEff now
    H.modify (start t) 
    pure next
  -- The user has requested to stop the clock
  eval (Stop next) = do
    t <- liftEff now 
    H.modify (stop t) 
    pure next
  -- The runtime system renders a clock tick
  eval (Refresh next) = do
    t <- liftEff now 
    H.modify (refresh t) 
    pure next
  -- The user has requested to open a new interval beginning at a
  -- specific point on the timeline (mouse down)

  -- The user has requested to close the currently open interval
  -- at a specific point on the timeline (mouse up)
  -- 



start :: Instant -> TimelineState -> TimelineState
start t s = 
  { limits: s.limits
  , history: s.history
  , active: s.active <|> Just { start: t, end: t }
  }

stop :: Instant -> TimelineState -> TimelineState
stop t s = 
  { limits: s.limits
  , history: maybe s.history (\st -> cons { start: st.start, end: t } s.history) s.active
  , active: Nothing
  }

refresh :: Instant -> TimelineState -> TimelineState
refresh t s = 
  { limits: s.limits
  , history: s.history
  , active: map (\a -> { start: a.start, end: t }) s.active
  }

ilen :: Instant -> Instant -> Number
ilen d d' = 
  let n (Milliseconds x) = x
  in  abs $ n (unInstant d) - n (unInstant d')

