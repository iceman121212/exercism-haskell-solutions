module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

{-
--what an embarassing typo in the last
--iteration; i think it's a sign
--that I ought to bump up my Emacs
--font size...

--Cleaning up the `isAnagram` logic
--which I overlooked in the last
--iteration...
-}

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram
  where clean          = map toLower
        std            = sort . clean
        cleanWord      = clean word
        stdWord        = std word
        isAnagram xs   = clean xs /= cleanWord &&
                           std xs == stdWord
                              
                              

