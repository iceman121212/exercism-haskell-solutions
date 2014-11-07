module Bob (responseFor) where

import Data.Char (toUpper)
import Text.Regex.Posix
  
responseFor :: String -> String
responseFor xs
  | isQuestion = "Sure."
  | isAddressedButNothingSaid = "Fine. Be that way!"
  | isYelling = "Whoa, chill out!"
  | otherwise = "Whatever."
  
  where
    isQuestion = endsInQuestionMark && (not isAllCaps)
    isAddressedButNothingSaid = onlySpaces || (noPunctuation && noConsecutiveLetters)
    isYelling = isAllCaps || (endsInExclamationMark && numCaps > 1 && no_e)
    
    endsInQuestionMark = xs =~ ".*\\?$" :: Bool
    isAllCaps = originalEqualsAllCaps && hasLetters
    originalEqualsAllCaps = xs == map toUpper xs
    hasLetters = xs =~ "[a-zA-Z]" :: Bool

    onlySpaces = foldr (\x acc -> if (x /= ' ') || (acc == False) then False else True) True xs
    noPunctuation = not hasPunctuation
    noConsecutiveLetters = not hasConsecutiveLetters

    endsInExclamationMark = xs =~".*!$" :: Bool
    no_e = not $ '\228' `elem` xs
    numCaps = foldr (\x acc -> if x `elem` ['A'..'Z'] then acc + 1 else acc) 0 xs
    
    hasConsecutiveLetters = xs =~ "[a-zA-Z][a-zA-Z]" :: Bool
    hasPunctuation = xs =~ "[,!\\?\\.]" :: Bool
                     
                        
                        


              
  
  
                             
