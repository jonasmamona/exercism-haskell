module Clock (addDelta, fromHourMin, toString) where

newtype Hours = Hours {getHours :: Int} deriving (Show)

newtype Minutes = Minutes {getMinutes :: Int} deriving (Eq, Show)

data Clock = Clock {getClockHours :: Hours, getGetClockMinutes :: Minutes}
  deriving (Show)

instance Eq Clock where
  (Clock (Hours x) (Minutes y)) == (Clock (Hours a) (Minutes b)) = (x == a) && (y == b)

minutesToInt :: Minutes -> Int
minutesToInt (Minutes x) = x

addMinutesWithCarryoverHours :: Minutes -> Minutes -> (Minutes, Hours)
addMinutesWithCarryoverHours (Minutes existingMinutes) (Minutes minutesToAddOrSubtract) = result
  where
    sumOfMinutes = existingMinutes + minutesToAddOrSubtract
    (newMinutes, carryoverHours) =
      if sumOfMinutes >= 0
        then (Minutes (sumOfMinutes `mod` 60), Hours (sumOfMinutes `div` 60))
        else (Minutes (60 + sumOfMinutes `mod` 60), Hours ((sumOfMinutes `div` 60) - 1))
    result = (newMinutes, carryoverHours)

addHours :: Hours -> Hours -> Hours
addHours (Hours x) (Hours y) = Hours $ (x + y) `mod` 24

addMinutes :: Minutes -> Minutes -> Minutes
addMinutes (Minutes x) (Minutes y) = Minutes $ (x + y) `mod` 60

showHours :: Clock -> String
showHours x
  | hours == 24 = "00"
  | hours < 10 = "0" ++ show hours
  | otherwise = show hours
  where
    hours = getHours $ getClockHours x

showMinutes :: Clock -> String
showMinutes x = if minutes < 10 then "0" ++ show minutes else show minutes
  where
    minutes = getMinutes $ getGetClockMinutes x

toString :: Clock -> String
toString clock = showHours clock ++ [':'] ++ showMinutes clock

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock hourResult (fst minutesWithRollover)
  where
    hourWithRollover = addHours (Hours hour) (Hours 0)
    minutesWithRollover = addMinutesWithCarryoverHours (Minutes min) (Minutes 0)
    hourCarryover = snd minutesWithRollover
    hourResult = addHours hourWithRollover hourCarryover

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = result
  where
    currentHours = getClockHours clock
    currentMinutes = getGetClockMinutes clock
    newMinutesAndCarryover = addMinutesWithCarryoverHours currentMinutes (Minutes min)
    newHours = addHours currentHours $ addHours (Hours hour) (snd newMinutesAndCarryover)
    result = Clock newHours (fst newMinutesAndCarryover)