module Aftok.HTML.Properties where

import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (IProp, attr)

dataToggle :: forall r i. String -> IProp r i
dataToggle = attr (AttrName "data-toggle")

dataTarget :: forall r i. String -> IProp r i
dataTarget = attr (AttrName "data-target")

dataDismiss :: forall r i. String -> IProp r i
dataDismiss = attr (AttrName "data-dismiss")

