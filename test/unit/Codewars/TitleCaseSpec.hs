module Codewars.TitleCaseSpec where

import Codewars.TitleCase (titleCase)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "titleCase" $ do
    it "Example test" $ do
      titleCase "a an the of" "a clash of KINGS" `shouldBe` "A Clash of Kings"
