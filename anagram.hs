module Anagram (anagramsFor) where

import Data.Char
import Data.List (sort)

stringToUpper :: String -> String
stringToUpper = map toUpper

isAnagram :: String -> String -> Bool
isAnagram x y = not isSameWord && isAnagram
  where
    isSameWord = stringToUpper x == stringToUpper y
    isAnagram = sort (stringToUpper x) == sort (stringToUpper y)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = foldr (\x acc -> if isAnagram xs x then acc ++ [x] else acc) []