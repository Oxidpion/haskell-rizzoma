module Codewars.TitleCase where

import Data.Char (toLower, toUpper)

-- https://www.codewars.com/kata/5202ef17a402dd033c000009/train/haskell
titleCase :: String -> String -> String
titleCase minor = capitalize . unwords . map (\x -> if isCapitalize x then capitalize x else x) . words . map toLower
  where
    isCapitalize = flip notElem (words $ map toLower minor)

    capitalize "" = ""
    capitalize (x : xs) = toUpper x : xs