module Codewars.Disemvowel where

import Data.Char ( toLower )

disemvowel :: String -> String
disemvowel = filter (noVolwes . toLower) where
    noVolwes x = x `notElem` "aouie"
