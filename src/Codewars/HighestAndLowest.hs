module Codewars.HighestAndLowest where

highAndLow :: String -> String
highAndLow input = unwords [maxNum, minNum]
  where
    numbers :: [Int]
    numbers = map read $ words input

    maxNum :: String
    maxNum = show $ maximum numbers

    minNum :: String
    minNum = show $ minimum numbers
