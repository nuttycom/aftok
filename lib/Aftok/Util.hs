{-# LANGUAGE RankNTypes #-}

module Aftok.Util where

import           ClassyPrelude
import           Control.Monad.Free.Church
import           Data.Functor.Coyoneda

type Program f a = F (Coyoneda f) a

-- Shouldn't this exist already in a library somewhere?
interpret :: Monad m => (forall x. f x -> m x) -> Program f a -> m a
interpret nt p =
  let eval (Coyoneda cf cm) = nt cm >>= cf
  in  iterM eval p

fc :: f a -> Program f a
fc = liftF . liftCoyoneda
