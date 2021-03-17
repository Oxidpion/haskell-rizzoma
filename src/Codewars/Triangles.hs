module Codewars.Triangles where

import Data.List (sort)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = 
    let [small, medium, big] = sort [a, b, c]
    in small + medium > big

