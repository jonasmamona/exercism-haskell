module Anagram (anagramsFor) where

import Data.Char
import Data.List (sort)

validAlphabeticCharacters :: [Char]
validAlphabeticCharacters = ['A' .. 'Z'] ++ ['a' .. 'z']

subject :: String
subject = "orchestra"

candidates :: [String]
candidates = ["cashregister", "Carthorse", "radishes"]

stringToUpper :: String -> String
stringToUpper = map toUpper

isAnagram :: String -> String -> Bool
isAnagram x y = not isSameWord && not isEmpty && hasSameLength && isAnagram
  where
    hasSameLength = length x == length y
    isSameWord = stringToUpper x == stringToUpper y
    isEmpty = null x
    isAnagram = sort (stringToUpper x) == sort (stringToUpper y)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = foldr (\x acc -> if isAnagram xs x then acc ++ [x] else acc) []