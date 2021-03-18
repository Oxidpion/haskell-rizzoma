module Codewars.HighestAndLowestSpec where

import Codewars.HighestAndLowest (highAndLow)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Example Tests" $ do
    it "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" $
      highAndLow "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" `shouldBe` "542 -214"