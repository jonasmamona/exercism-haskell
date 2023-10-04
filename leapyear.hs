module LeapYear where

isEvenlyDivisible :: Integer -> Integer -> Bool
isEvenlyDivisible dividend divisor = mod dividend divisor == 0

isLeapYear :: Integer -> Bool
isLeapYear year = isEvenlyDivisible year 4 && (not(isEvenlyDivisible year 100) || isEvenlyDivisible year 400)