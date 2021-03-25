{-# LANGUAGE TupleSections #-}

module Tmorris where

-- https://blog.tmorris.net/posts/20-intermediate-haskell-exercises/
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just a) = Just $ f a
  furry _ Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)

newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft $ Left $ f a
  furry f (EitherLeft (Right b)) = EitherLeft $ Right b

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left a)) = EitherRight $ Left a
  furry f (EitherRight (Right b)) = EitherRight $ Right $ f b

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn a = [a]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just a) = f a
  banana f Nothing = Nothing
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g t = f (g t) t
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left a)) = f a
  banana f (EitherLeft (Right b)) = EitherLeft $ Right b
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Left a)) = EitherRight $ Left a
  banana f (EitherRight (Right b)) = f b
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = banana . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (a : as) f = banana (\x -> furry' (x :) $ moppy as f) (f a)

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ms = moppy ms id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb $ furry' f ma

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc $ banana2 f ma mb

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f a b c d = apple d $ banana3 f a b c

newtype State s a = State
  { state :: s -> (s, a)
  }

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f st = State (\x -> let (s, a) = state st x in (s, f a))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f sa = State (\x -> let (_, a) = state sa x in state (f a) x)
  unicorn a = State (,a)