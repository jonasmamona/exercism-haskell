module SumOfMultiples (sumOfMultiples) where

import Data.List

removeNonUnique :: [Integer] -> [Integer]
removeNonUnique [] = []
removeNonUnique (x:xs)
    | x `elem` xs = removeNonUnique xs
    | otherwise = x : removeNonUnique xs

getMultiplesBelowLimit :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
getMultiplesBelowLimit baseValue limit current result
  | isBaseAboveLimit = [0]
  | current >= limit = result
  | otherwise = getMultiplesBelowLimit baseValue limit (current + baseValue) (result ++ [current])
  where
      isBaseAboveLimit = null result && baseValue > limit

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ removeNonUnique $ concat $ foldr (\x acc -> acc ++ [getMultiplesBelowLimit x limit 0 []]) [] factors