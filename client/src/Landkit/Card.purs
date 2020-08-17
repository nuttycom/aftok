module Landkit.Card where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as P

component ::
  forall action slots m.
  H.ComponentHTML action slots m ->
  H.ComponentHTML action slots m
component child =
  HH.div
    [ P.classes (ClassName <$> ["card", "card-row"]) ]
    [ child ]
