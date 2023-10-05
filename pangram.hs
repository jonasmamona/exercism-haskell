module Pangram (isPangram) where
import Data.Char (toLower)

alphabet :: String
alphabet = ['a'..'z']

sampleString :: String
sampleString = "The quick brown fox jumps over the lazy dog."

lowerString :: String -> String
lowerString = map toLower

removeCharFromString :: Char -> String -> String
removeCharFromString character = foldr (\x acc -> if x /= character then x : acc else acc) []

removeWhiteSpaceFromString :: String -> String
removeWhiteSpaceFromString = removeCharFromString ' '

cleanString :: String -> String
cleanString = removeWhiteSpaceFromString . lowerString

calculateRemainderChars :: String -> String -> String
calculateRemainderChars [] acc = acc
calculateRemainderChars list@(x:xs) acc = calculateRemainderChars xs (removeCharFromString x acc)

shortIsPangram :: String -> Bool
shortIsPangram text = all (`elem` lowerString text) alphabet

isPangram :: String -> Bool
isPangram text = calculateRemainderChars (cleanString text) alphabet == ""