module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples divisors limit = sum $ filter divisible [1..limit-1]
  where divisible n = any (==0) $ map (rem n) divisors

sumOfMultiplesDefault :: Int -> Int        
sumOfMultiplesDefault = sumOfMultiples [5,3]
                        

