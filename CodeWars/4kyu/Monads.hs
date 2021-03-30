{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State (\s -> (x, s))
  (State g) >>= f = State (\s -> let (x, s') = g s in runState (f x) s') 

instance Monad (Reader s) where
  return x = Reader (\s -> x)
  (Reader g) >>= f = Reader (\s -> runReader (f $ g s) s)

instance Monoid w => Monad (Writer w) where
  return s = Writer (mempty, s)
  (Writer (s, v)) >>= f = let (s', v') = runWriter (f v) in Writer (s `mappend` s', v')
