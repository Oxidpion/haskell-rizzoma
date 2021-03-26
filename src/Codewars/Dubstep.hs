module Codewars.Dubstep where

import Data.List.Split (splitOn)

-- https://www.codewars.com/kata/551dc350bf4e526099000ae5/train/haskell
songDecoder :: String -> String
songDecoder = unwords . filter (not . null) . splitOn "WUB"
