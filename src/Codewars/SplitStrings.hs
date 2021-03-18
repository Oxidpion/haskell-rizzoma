module Codewars.SplitStrings where

solution :: String -> [String]
solution [] = []
solution [a] = [[a, '_']]
solution (a : b : ss) = [a, b] : solution ss