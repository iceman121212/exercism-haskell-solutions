module DNA (toRNA) where

toRNA :: [Char] -> [Char]
toRNA = map transcribe

transcribe :: Char -> Char
transcribe x
  | x == 'G' = 'C'
  | x == 'C' = 'G'
  | x == 'A' = 'U'
  | x == 'T' = 'A'
  | otherwise = ' '
