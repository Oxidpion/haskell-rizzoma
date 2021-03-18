module Codewars.DisemvowelSpec where

import Codewars.Disemvowel (disemvowel)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should work for single words" $ do
    disemvowel "hat" `shouldBe` "ht"
    disemvowel "toast" `shouldBe` "tst"
  it "should work with spaces" $ do
    disemvowel "toast hat" `shouldBe` "tst ht"
