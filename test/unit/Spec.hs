{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Spec where

import Test.Hspec.Discover
    ( hspec, describe, Spec, postProcessSpec )

main :: IO ()
main = hspec spec