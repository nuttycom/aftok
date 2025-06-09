module Aftok.Modals.ModalFFI
  ( toggleModal
  , Toggle(..)
  ) where

import Prelude (Unit)
import Effect (Effect)

data Toggle
  = ShowModal
  | ToggleModal
  | HideModal

toggleModal :: String -> Toggle -> Effect Unit
toggleModal modalId t =
  let
    toggleStr = case t of
      ShowModal -> "show"
      ToggleModal -> "toggle"
      HideModal -> "hide"
  in
    toggleModalInternal modalId toggleStr

foreign import toggleModalInternal :: String -> String -> Effect Unit

