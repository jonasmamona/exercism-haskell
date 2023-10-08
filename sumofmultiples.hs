module SumOfMultiples where

import Data.List
import Data.Map (Map, empty, fromList, insert, insertWith, toList)

getMultiplesBelowLimit :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
getMultiplesBelowLimit baseValue limit current result
  | isBaseAboveLimit = [0]
  | current >= limit = result
  | otherwise = getMultiplesBelowLimit baseValue limit (current + baseValue) (result ++ [current])
  where
    isBaseAboveLimit = null result && baseValue > limit

insertListOfTuplesIntoMap :: [(Integer, Integer)] -> Map Integer Integer -> Map Integer Integer
insertListOfTuplesIntoMap listOfTuples result = foldr (uncurry Data.Map.insert) result listOfTuples

combineMaps :: Map Integer Integer -> [Map Integer Integer] -> Map Integer Integer
combineMaps = foldr (insertListOfTuplesIntoMap . Data.Map.toList)

convertIntegerListIntoListOfTuples :: [Integer] -> [(Integer, Integer)]
convertIntegerListIntoListOfTuples = map (\x -> (x,x))

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum $ map fst $ Data.Map.toList $ Data.Map.fromList $ convertIntegerListIntoListOfTuples generatedMaps
    where generatedMaps = concat $ foldr (\x acc -> acc ++ [getMultiplesBelowLimit x limit 0 []]) [] factors

testCombineMaps = combineMaps (fromList [(1, 1), (2, 2), (3, 3)]) [fromList [(4, 4), (5, 5), (3, 3)], fromList [(4, 4), (5, 5), (3, 3)], fromList [(10, 10), (11, 11), (500, 500)]]

sumOfMultiplesNotStupid :: [Integer] -> Integer -> Integer
sumOfMultiplesNotStupid multiples limit =
    sum [x | x <- [1..limit - 1], any (\m -> x `mod` m == 0) multiplesWithNoZero]
    where multiplesWithNoZero = filter (/= 0) multiplesWithNoZero