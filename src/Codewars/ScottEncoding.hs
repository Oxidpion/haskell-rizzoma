{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- https://www.codewars.com/kata/scott-encoding/train/haskell
module Codewars.ScottEncoding where

import Prelude hiding (concat, curry, foldl, foldr, fst, length, map, null, snd, take, uncurry, zip, (++))

newtype SPair a b = SPair {runPair :: forall c. (a -> b -> c) -> c}

toPair :: SPair a b -> (a, b)
toPair p = (fst p, snd p)

fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b

fst :: SPair a b -> a
fst p = runPair p const

snd :: SPair a b -> b
snd p = runPair p (\_ x -> x)

swap :: SPair a b -> SPair b a
swap p = SPair $ \f -> runPair p (flip f)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry fp a b = fp (SPair $ \f -> f a b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry = flip runPair

newtype SMaybe a = SMaybe {runMaybe :: forall b. b -> (a -> b) -> b}

toMaybe :: SMaybe a -> Maybe a
toMaybe sm = runMaybe sm Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just a) = SMaybe $ \_ f -> f a

isJust :: SMaybe a -> Bool
isJust sm = runMaybe sm False (const True)

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

newtype SEither a b = SEither {runEither :: forall c. (a -> c) -> (b -> c) -> c}

toEither :: SEither a b -> Either a b
toEither ei = runEither ei Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither $ \f _ -> f a
fromEither (Right b) = SEither $ \_ f -> f b

isLeft :: SEither a b -> Bool
isLeft ei = runEither ei (const True) (const False)

isRight :: SEither a b -> Bool
isRight = not . isLeft

newtype SList a = SList {runList :: forall b. b -> (a -> SList a -> b) -> b}

toList :: SList a -> [a]
toList sl = runList sl [] $ \a sla -> a : toList sla

fromList :: [a] -> SList a
fromList [] = SList const
fromList (a : as) = SList $ \_ f -> f a $ fromList as

cons :: a -> SList a -> SList a
cons a sl = SList $ \_ f -> f a sl

concat :: SList a -> SList a -> SList a
concat sl1 sl2 = runList sl1 sl2 $ \x sl -> cons x $ concat sl sl2

null :: SList a -> Bool
null sl = runList sl True $ \_ _ -> False

length :: SList a -> Int
length sl = runList sl 0 $ \_ s -> 1 + length s

map :: (a -> b) -> SList a -> SList b
map f sla = runList sla (SList const) $ \a sl -> cons (f a) $ map f sl

zip :: SList a -> SList b -> SList (SPair a b)
zip sl1 sl2 = runList sl1 (SList const) $
  \a sl01 -> runList sl2 (SList const) $
    \b sl02 -> cons (fromPair (a, b)) $ zip sl01 sl02

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b sla = runList sla b $ \a sl -> foldl f (f b a) sl

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b sla = runList sla b $ \a sl -> f a $ foldr f b sl

take :: Int -> SList a -> SList a
take n sl
  | n > 0 = runList sl (SList const) $ \x s -> cons x $ take (n - 1) s
  | otherwise = SList const

catMaybes :: SList (SMaybe a) -> SList a
catMaybes sla = runList sla (SList const) $
  \sm sl -> runMaybe sm (catMaybes sl) $ \a -> cons a $ catMaybes sl

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr f (fromPair (SList const, SList const))
  where
    f ei p =
      runEither
        ei
        (\a -> fromPair (cons a (fst p), snd p))
        (\b -> fromPair (fst p, cons b (snd p)))