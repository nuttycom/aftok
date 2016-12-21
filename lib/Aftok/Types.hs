{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Aftok.Types (Satoshi(..), satoshi) where

import           ClassyPrelude
import           Control.Lens
import           Network.Bippy.Types (Satoshi(..))

satoshi :: Lens' Satoshi Word64 
satoshi inj (Satoshi value) = Satoshi <$> inj value

