module Phone (number, areaCode, prettyPrint) where

import Data.Char (isDigit)
  
number :: String -> String
number ns
  | len == 10 = num
  | len == 11 && head num == '1' = tail num
  | otherwise = replicate 10 '0'
  where
    num = getDigits ns
    len = length num
    getDigits = foldr (\x acc ->
                        if isDigit x
                          then (x:acc)
                          else acc) []
                        
areaCode :: String -> String
areaCode = take 3 . number
           
prettyPrint :: String -> String
prettyPrint n =
  let num = number n
      firstThree = "(" ++ take 3 num ++ ")"
      middleThree = " " ++ (take 3 $ drop 3 num)
      lastFour    = "-" ++ drop 6 num
  in  firstThree ++ middleThree ++ lastFour
