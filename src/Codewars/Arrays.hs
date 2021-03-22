module Codewars.Arrays where

positiveSum :: [Int] -> Int
positiveSum ns = sum [n | n <- ns, n > 0]
