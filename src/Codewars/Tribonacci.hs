module Codewars.Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ n | n < 1 = []
tribonacci (a, b, c) n = a : tribonacci (b, c, a + b + c) (n -1)
