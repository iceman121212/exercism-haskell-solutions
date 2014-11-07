module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear n
  | (n `rem` 100) == 0 && ( n `rem` 400) /= 0 = False
  | n `rem` 4 == 0 = True
  | otherwise = False

