module Aftok.HTML.QRious
  ( renderQR
  , QROpts
  )
  where

import Effect (Effect)

renderQR :: QROpts -> Effect String
renderQR = renderQRInternal

type QROpts = 
  { value :: String
  , size :: Int
  }

foreign import renderQRInternal :: QROpts -> Effect String
