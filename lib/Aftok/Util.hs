{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Aftok.Util where

import Control.Error.Util (maybeT)
import Control.Monad.Free.Church
import Data.Functor.Coyoneda
import Data.Map.Strict as M
import qualified Debug.Trace as T

newtype Program (f :: * -> *) (a :: *)
  = Program
      {runProgram :: F (Coyoneda f) a}
  deriving (Functor, Applicative, Monad)

-- Shouldn't this exist already in a library somewhere?
interpret :: Monad m => (forall x. f x -> m x) -> Program f a -> m a
interpret nt p =
  let eval (Coyoneda cf cm) = nt cm >>= cf in iterM eval (runProgram p)

fc :: f a -> Program f a
fc = Program . liftF . liftCoyoneda

traverseKeys :: (Ord k, Applicative f) => (a -> f k) -> Map a b -> f (Map k b)
traverseKeys f m =
  let insf a b m' = flip insert b <$> f a <*> m'
   in foldrWithKey insf (pure M.empty) m

fromMaybeT :: (Monad m) => m a -> MaybeT m a -> m a
fromMaybeT a m = maybeT a pure m

traceWith :: Show b => (a -> b) -> a -> a
traceWith f a = T.trace (show $ f a) a
