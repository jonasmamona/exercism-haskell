module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, fromList, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

convertCharIntoNucleotide :: Char -> Either String Nucleotide
convertCharIntoNucleotide dna
  | dna == 'G' = Right G
  | dna == 'C' = Right C
  | dna == 'T' = Right T
  | dna == 'A' = Right A
  | otherwise = Left "Not a valid nucleotide"

nucleotideMap :: (Map Nucleotide Int)
nucleotideMap = fromList [(G, 0), (C, 0), (T, 0), (A, 0)]

foldNucleotides :: String -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
foldNucleotides [] nucleotideMap = Right nucleotideMap
foldNucleotides list@(x:xs) nucleotideMap = 
    case convertCharIntoNucleotide x of
        Right convertedNucleotide -> foldNucleotides xs (insertWith (+) convertedNucleotide 1 nucleotideMap)
        Left error -> Left error

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldNucleotides xs nucleotideMap