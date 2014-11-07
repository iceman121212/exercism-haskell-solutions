module WordCount (wordCount) where

--Changed the name of helper function
--from `split` back to `makeCountable`
--to avoid confusion with functions
--in the Data.List.Split library

import Data.Char (toLower, isAlphaNum)
import Data.List.Split (wordsBy)
import Data.Map (Map, fromListWith)

wordCount :: String -> Map String Int
wordCount = fromListWith (+) . makeCountable . map toLower
  where makeCountable = flip zip (repeat 1). wordsBy (not . isAlphaNum)
