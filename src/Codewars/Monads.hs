{-# LANGUAGE NoImplicitPrelude #-}

-- https://www.codewars.com/kata/547202bdf7587835d9000c46/haskell
module Codewars.Monads where

import Data.Monoid (Monoid (mappend, mempty))
import Prelude hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a}

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return = State . (,)
  (State g) >>= f = State $ \s -> let (a, s') = g s in runState (f a) s'

instance Monad (Reader s) where
  return = Reader . const
  (Reader g) >>= f = Reader $ \s -> runReader (f $ g s) s

instance Monoid w => Monad (Writer w) where
  return = Writer . (,) mempty
  (Writer (s, v)) >>= f = let Writer (s', b) = f v in Writer (mappend s s', b)
