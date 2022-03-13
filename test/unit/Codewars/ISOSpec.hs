module Codewars.ISOSpec where

import Codewars.ISO

import Test.Hspec
import Test.QuickCheck
import Data.Either

bISO :: ISO Bool Bool
bISO = (not, not)

lrl :: ISO a b -> (a -> a)
lrl (ab, ba) = ba . ab

rlr :: ISO a b -> (b -> b)
rlr (ab, ba) = ab . ba

str :: String
str = "JoJo"

pa :: ISO
  (Either (Either Int Bool) String)
  (Either Int (Either Bool String))
pa = plusAssoc

spec = do
  describe "subst" $ do
    it "substL" $ do
      substL bISO    True  `shouldBe` False
      substL bISO    False `shouldBe` True
      substL isoBool False `shouldBe` False
      substL isoBool True  `shouldBe` True
    it "substR" $ do
      substR bISO    True  `shouldBe` False
      substR bISO    False `shouldBe` True
      substR isoBool True  `shouldBe` True
      substR isoBool False `shouldBe` False
    it "isoEU" $ do
      isLeft (substL isoEU (Right ())) `shouldBe` True
    it "lrl isoEU (Left (replicate n ())) == Left (replicate n ())" $
      property $ \(NonNegative n) -> 
        lrl isoEU (Left (replicate n ())) == Left (replicate n ())
    
  describe "isoLaw" $ do
    it "assoc" $ do
      lrl pa (Left  (Left     0)) `shouldBe` Left  (Left     0)
      lrl pa (Left  (Right True)) `shouldBe` Left  (Right True)
      lrl pa (Right          str) `shouldBe` Right          str
      rlr pa (Left             0) `shouldBe` Left             0
      rlr pa (Right (Left  True)) `shouldBe` Right (Left  True)
      rlr pa (Right (Right  str)) `shouldBe` Right (Right  str)
