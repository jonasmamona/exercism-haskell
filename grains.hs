module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n = if n > 64 || n <= 0 then Nothing else internalFunction n 1
  where
    internalFunction 0 result = Just result
    internalFunction iterations result = operation
      where
        operation =
          if n == iterations
            then internalFunction (iterations - 1) result
            else internalFunction (iterations - 1) (result * 2)

total :: Integer
total = sum $ executeOperations 64 []
  where
    executeOperations :: Integer -> [Integer] -> [Integer]
    executeOperations n result =
      if n > 0
        then executeOperations (n - 1) (fromJust (square n) : result)
        else result
