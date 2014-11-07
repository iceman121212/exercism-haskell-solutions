module DNA (count, nucleotideCounts) where

import Data.Map.Strict (Map, update, fromList)
  
nucleotideCounts :: String -> Map Char Int
nucleotideCounts = foldr (update increment) (initCount 0)
  where
    initCount = fromList . zip "ATCG" . repeat
    increment value = Just (value + 1)

count :: Char -> String -> Int
count nuc
  | nuc `elem` "ATCG" = length . filter (==nuc)
  | otherwise = error errorMsg
  where errorMsg = "invalid nucleotide " ++ show nuc
