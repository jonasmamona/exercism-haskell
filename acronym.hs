module Acronym (abbreviate) where

import Data.Char (isLetter, isLower, isUpper, toUpper)

swapHyphenForSpace :: [Char] -> [Char]
swapHyphenForSpace = map (\x -> if x == '-' then ' ' else x)

removeNonLetters :: [Char] -> [Char]
removeNonLetters = filter (\x -> isLetter x || x == ' ')

getHeadAndUppercase :: String -> String
getHeadAndUppercase word@(x : xs) = if all isUpper word then [x] else toUpper x : filter isUpper xs

abbreviate :: [Char] -> [Char]
abbreviate xs = concatMap getHeadAndUppercase $ words $ removeNonLetters $ swapHyphenForSpace xs