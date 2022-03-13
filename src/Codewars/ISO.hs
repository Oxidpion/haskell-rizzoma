module Codewars.ISO where

import Data.Void (absurd, Void)
-- https://www.codewars.com/kata/5922543bf9c15705d0000020/train/haskell
-- 
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (f1, f2) = (f2, f1)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) =
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (ac2bd, bd2ac)
  where
    ac2bd (Left a) = Left $ ab a
    ac2bd (Right c) = Right $ cd c

    bd2ac (Left b) = Left $ ba b
    bd2ac (Right d) = Right $ dc d

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba , \bd -> dc . bd . ab)

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mamb, mbma) = (getB . mamb . Just, getA . mbma . Just)
  where
    getB (Just x) = x
    getB Nothing = getB $ mamb Nothing

    getA (Just x) = x
    getA Nothing = getA $ mbma Nothing
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (fev, fve)
  where
    fve (Left []) = Right ()
    fve (Left xs) = Left $ tail xs
    fve (Right x) = Right $ absurd x

    fev (Left xs) = Left (() : xs)
    fev (Right ()) = Left []

-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
-- iab -> iba, iba -> iab
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)


-- https://www.codewars.com/kata/5917f22dd2563a36a200009c/train/haskell

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (swap, swap)
  where
    swap (Right x) = Left x
    swap (Left x) = Right x

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (func1, func2)
  where
    func1 :: Either (Either a b) c -> Either a (Either b c)
    func1 (Right x) = Right $ Right x
    func1 (Left (Right x)) = Right $ Left x
    func1 (Left (Left x)) = Left x

    func2 :: Either a (Either b c) -> Either (Either a b) c
    func2 (Left x) = Left $ Left x
    func2 (Right (Left x)) = Left $ Right x
    func2 (Right (Right x)) = Right x

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (uncurry $ flip (,), uncurry $ flip (,))

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (func1, func2)
  where
    func1 ((a, b), c) = (a, (b, c))
    func2 (a, (b, c)) = ((a, b), c)

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (func1, func2)
  where
    func1 (a, e) = case e of
      Left b -> Left (a, b)
      Right c -> Right (a, c)

    func2 (Left (a, b)) = (a, Left b)
    func2 (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (func1, func2)
  where
    func1 True = Just Nothing
    func1 False = Nothing

    func2 Nothing = False
    func2 (Just _) = True


-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (func1, func2)
  where
    func1 (Right b) = Just $ Right b
    func1 (Left Nothing) = Nothing
    func1 (Left (Just a)) = Just $ Left a

    func2 Nothing = Left Nothing
    func2 (Just (Left a)) = Left $ Just a
    func2 (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (fst, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (func1, func2)
  where
    func1 (Nothing, b) = Left b
    func1 (Just a, b) = Right (a, b)

    func2 (Left b) = (Nothing, b)
    func2 (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans`
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (func1, func2)
  where
    func1 fmb = (fmb Nothing, fmb . Just)
    func2 (a, f) Nothing = a
    func2 (a, f) (Just b) = f b

-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO =
  isoPow one refl `trans`
    powS `trans`
    isoProd refl powO `trans`
    multComm `trans`
    multSO

-- Here's a trick: 
-- replace undefined with the rhs of the comment on previous line
-- When you're not sure what to fill in for a value,
-- Have it as a _
-- GHC will goes like
-- "Found hole `_' with type: ISO (() -> a) (Maybe b0 -> a0)"
-- So you can immediately see value of what type are needed
-- This process can be repeat indefinitely -
-- For example you might replace `_` with `isoFunc _ _`
-- So GHC hint you on more specific type.
-- This is especially usefull if you have complex type.
-- See https://wiki.haskell.org/GHC/Typed_holes
-- And "stepwise refinement" for more details.