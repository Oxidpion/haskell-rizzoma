module Codewars.DubstepSpec where

import Codewars.Dubstep (songDecoder)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "songDecoder" $ do
    it "should work for some examples" $ do
      songDecoder "AWUBBWUBC" `shouldBe` "A B C"
      songDecoder "AWUBWUBWUBBWUBWUBWUBC" `shouldBe` "A B C"
      songDecoder "WUBAWUBBWUBCWUB" `shouldBe` "A B C"
      songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
        `shouldBe` "WE ARE THE CHAMPIONS MY FRIEND"
      songDecoder "NEVERWUBWUBGONNAWUBGIVEWUBWUBYOUWUBWUBUPWUBWUBNEVERWUBWUBWUBWUBGONNAWUBWUBLETWUBWUBYOUWUBWUBDOWN"
        `shouldBe` "NEVER GONNA GIVE YOU UP NEVER GONNA LET YOU DOWN"