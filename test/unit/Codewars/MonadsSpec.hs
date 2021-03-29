module Codewars.MonadsSpec where

import Codewars.Monads
import Test.Hspec (Spec, describe, it, shouldBe)

-- Dummy test to make sure your code type checks before submission.
spec :: Spec
spec = do
  describe "Type Check" $ do
    it "type check" $ 1 `shouldBe` 1