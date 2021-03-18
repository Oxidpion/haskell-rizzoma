module Codewars.IsogramSpec where

import Codewars.Isogram (isIsogram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "isIsogram" $ do
    it "testing 'Dermatoglyphics'" $ shouldBe (isIsogram "Dermatoglyphics") True
    it "testing 'moOse'" $ shouldBe (isIsogram "moOse") False
    it "testing 'aba'" $ shouldBe (isIsogram "aba") False