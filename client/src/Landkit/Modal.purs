module Landkit.Modal where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as P

-- TODO: some sketchy stuff related to how this thing is supposed to be open
component
  :: forall action slots m
   . Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
component children =
  HH.div
    [ P.classes (ClassName <$> [ "modal", "fade" ])
    , P.id "modalSigninHorizontal" -- TODO: is this needed? is this bootstrap magic?
    , P.tabIndex (-1)
    -- , P.role "dialog"
    -- , P.labelledby "modalSigninHorizontalTitle"
    -- , P.ariaHidden "true"
    ]
    [ HH.div
        [ P.classes (ClassName <$> [ "modal-dialog", "modal-lg", "modal-dialog-centered" ])
        -- , P.role "document"
        ]
        [ HH.div
            [ P.classes (ClassName <$> [ "modal-content" ]) ]
            children
        ]
    ]
