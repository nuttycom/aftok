module Aftok.HTML.KjuaQR 
  ( QRType(..)
  , QROpts
  , renderQR
  )
  where

import Prelude
import Effect (Effect)

data QRType
  = Canvas
  | Image
  | SVG

data ErrorCorrection = L | M | Q | H

renderQR :: String -> QROpts -> Effect Unit
renderQR = renderQRInternal

type QROpts = 
  { content :: String
  }
--     -- render method: 'canvas', 'image' or 'svg'
--     render :: QRType,
--     -- render pixel-perfect lines
--     crisp :: Boolean,
--     -- minimum version: 1..40
--     minVersion :: Int,
--     -- error correction level: 'L', 'M', 'Q' or 'H'
--     ecLevel :: ErrorCorrection,
--     -- size in pixel: 200
--     size :: Int,
--     -- pixel-ratio, null for devicePixelRatio
--     -- ratio :: null,
-- 
--     --code color: '#333',
--     fill :: String -- hack, fine for now
--     -- background color '#fff'
--     back :: String,
-- 
--     -- content
--     text :: String,
-- 
--     -- roundend corners in pc: 0..100
--     rounded: Int,
-- 
--     -- quiet zone in modules: 0
--     quiet: Int,
-- 
--     -- modes: 'plain', 'label' or 'image'
--     mode: 'plain',
-- 
--     -- label/image size and pos in pc: 0..100
--     mSize: 30,
--     mPosX: 50,
--     mPosY: 50,
-- 
--     -- label
--     label: 'no label',
--     fontname: 'sans',
--     fontcolor: '#333',
-- 
--     -- image element
--     image: null
-- 

-- type QROptsInternal = 
--   {
--     -- render method: 'canvas', 'image' or 'svg'
--     render :: String,
-- 
--     -- render pixel-perfect lines
--     crisp :: Boolean,
-- 
--     -- minimum version: 1..40
--     minVersion :: Int
-- 
--     -- error correction level: 'L', 'M', 'Q' or 'H'
--     ecLevel: 'L',
-- 
--     -- size in pixel
--     size: 200,
-- 
--     -- pixel-ratio, null for devicePixelRatio
--     ratio: null,
-- 
--     -- code color
--     fill: '#333',
-- 
--     -- background color
--     back: '#fff',
-- 
--     -- content
--     text: 'no text',
-- 
--     -- roundend corners in pc: 0..100
--     rounded: 0,
-- 
--     -- quiet zone in modules
--     quiet: 0,
-- 
--     -- modes: 'plain', 'label' or 'image'
--     mode: 'plain',
-- 
--     -- label/image size and pos in pc: 0..100
--     mSize: 30,
--     mPosX: 50,
--     mPosY: 50,
-- 
--     -- label
--     label: 'no label',
--     fontname: 'sans',
--     fontcolor: '#333',
-- 
--     -- image element
--     image: null
-- 
-- 
--   }

foreign import renderQRInternal :: String -> QROpts -> Effect Unit
