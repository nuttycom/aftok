{-# LANGUAGE RankNTypes #-}

module Aftok.Util where

import           ClassyPrelude
import           Control.Monad.Free.Church
import           Data.Functor.Coyoneda
import           Data.Map.Strict           as M

type Program f a = F (Coyoneda f) a

-- Shouldn't this exist already in a library somewhere?
interpret :: Monad m => (forall x. f x -> m x) -> Program f a -> m a
interpret nt p =
  let eval (Coyoneda cf cm) = nt cm >>= cf
  in  iterM eval p

fc :: f a -> Program f a
fc = liftF . liftCoyoneda

traverseKeys :: (Ord k, Applicative f) => (a -> f k) -> Map a b -> f (Map k b)
traverseKeys f m =
  let insf a b m' = flip insert b <$> f a <*> m'
  in  foldrWithKey insf (pure M.empty) m
