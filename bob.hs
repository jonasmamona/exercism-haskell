module Bob (responseFor) where

import Data.Char (isLetter, isNumber, isUpper, toUpper)
import Data.Monoid as M
import Data.Text as T
import Prelude as P

allCaps :: String
allCaps = ['A' .. 'Z']

removeCharFromText :: Char -> Text -> Text
removeCharFromText character = T.filter (/= character)

isAlphabetCharacter :: Char -> Bool
isAlphabetCharacter character = Data.Char.toUpper character `P.elem` allCaps

isQuestion :: Text -> Bool
isQuestion text = T.last (removeCharFromText ' ' text) == '?'

removeSpecialCharacters :: Text -> Text
removeSpecialCharacters = T.filter (\character -> (isLetter character || isNumber character) && character /= '?')

removeAllSpecialCharacters :: Text -> Text
removeAllSpecialCharacters = T.filter (\character -> isLetter character || isNumber character)

removeNonLetters :: Text -> Text
removeNonLetters = T.filter isLetter

removeTabsAndNewLines :: Text -> Text
removeTabsAndNewLines = T.filter (\character -> character /= '\t' && character /= '\n' && character /= '\r')

isYell :: Text -> Bool
isYell text = T.any isLetter text && T.all (\x -> isUpper x && isAlphabetCharacter x) (removeSpecialCharacters text)

isYellQuestion :: Text -> Bool
isYellQuestion text = isYell (removeAllSpecialCharacters text) && isQuestion text

isSilence :: Text -> Bool
isSilence text = T.null (removeCharFromText ' ' text)

responseFor :: Text -> Text
responseFor text
  | isSilence (removeTabsAndNewLines text) = pack "Fine. Be that way!"
  | isYellQuestion text = pack "Calm down, I know what I'm doing!"
  | isQuestion text = pack "Sure."
  | isYell (removeNonLetters text) = pack "Whoa, chill out!"
  | otherwise = pack "Whatever."