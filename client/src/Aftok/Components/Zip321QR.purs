module Aftok.Components.Zip321QR where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Unfoldable as U
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as P
import Aftok.Types (System)
import Aftok.Api.Types (Zip321Request)

type Input = Zip321Request

type CState = 
  { req :: Zip321Request
  , dataUrl :: Maybe String
  }

data Action 
  = QrInit

type Slot id
  = forall query. H.Slot query Unit id

component ::
  forall m output query.
  Monad m =>
  System m ->
  H.Component HH.HTML query Input output m
component system =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval 
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just QrInit
              }
    }
  where
  initialState :: Input -> CState
  initialState input = 
    { req: input, dataUrl: Nothing }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    HH.div_
      [ HH.div_
        ((\url -> HH.img [P.src url]) <$> U.fromMaybe st.dataUrl)
      , HH.div_
        [ HH.span
            [ P.classes (ClassName <$> ["code", "zip321uri"]) ]
            [HH.text <<< unwrap $ st.req]
        ]
      ]

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots output m Unit
  handleAction = case _ of
    QrInit -> do
      req <- H.gets (_.req)
      uri <- lift $ system.renderQR { value: unwrap req, size: 300 }
      H.modify_ (_ { dataUrl = Just uri })

