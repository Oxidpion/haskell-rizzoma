{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Spec where

import Test.Hspec.Discover
  ( Spec,
    describe,
    hspec,
    postProcessSpec,
  )

main :: IO ()
main = hspec spec