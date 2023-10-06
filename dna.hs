module DNA (toRNA) where

convertDnaIntoRna :: Char -> Either Char Char
convertDnaIntoRna dna
  | dna == 'G' = Right 'C'
  | dna == 'C' = Right 'G'
  | dna == 'T' = Right 'A'
  | dna == 'A' = Right 'U'
  | otherwise = Left dna

sequenceDna :: Either Char String -> String -> Either Char String
sequenceDna (Left x) _ = Left x
sequenceDna (Right []) rna = Right rna
sequenceDna (Right dna@(x : xs)) rna =
  case convertDnaIntoRna x of
    (Right result) -> sequenceDna (Right xs) (rna ++ [result])
    (Left result) -> Left result

toRNA :: String -> Either Char String
toRNA xs = sequenceDna (Right xs) ""