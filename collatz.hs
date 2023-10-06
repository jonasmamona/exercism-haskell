module CollatzConjecture (collatz) where


calculate :: Integer ->  Integer -> Integer
calculate 1 acc = acc
calculate number acc = calculate calculatedValue (acc+1)
    where
        calculatedValue = if even number then div number 2 else (3*number)+1


collatz :: Integer -> Maybe Integer
collatz 0 = Nothing
collatz n = if 0 > n then Nothing else Just (calculate n 0)