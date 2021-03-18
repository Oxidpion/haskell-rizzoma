module Codewars.Isogram where

import Data.Char (toLower)
import Data.List (sort)

isIsogram :: String -> Bool
isIsogram str = and $ zipWith (/=) ss (tail ss)
  where
    ss = sort $ map toLower str