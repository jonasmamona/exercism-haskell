module SpaceAge (Planet (..), ageOn) where
import Numeric

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune


earthYearInSeconds :: Float
earthYearInSeconds = 31557600.0

getPlanetOrbitalPeriod :: Planet -> Float
getPlanetOrbitalPeriod planet =
  case planet of
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.79132
    Earth -> 1.0

convertAgeToSeconds :: Float -> Float
convertAgeToSeconds age = age * earthYearInSeconds

convertSecondsToEarthYear :: Float -> Float
convertSecondsToEarthYear seconds = seconds / earthYearInSeconds

roundFloat :: Float -> Float
roundFloat x = read $ showFFloat (Just 2) x ""

ageOn :: Planet -> Float -> Float
ageOn planet seconds = roundFloat ((convertSecondsToEarthYear seconds / getPlanetOrbitalPeriod planet) * earthYearInSeconds)