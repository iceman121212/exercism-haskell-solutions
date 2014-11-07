module DNA (hammingDistance) where

{-
four versions:
 0) length + filter + zipWith
 1) sum + zipWith
 2) length + filter + zip
 3) sum + map + zip                    -}

--toggle:
hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance = hammingDistance0
----------------------------------------------------------------------------------------------------
hammingDistance0 :: (Eq a) => [a] -> [a] -> Int
hammingDistance0 xs = length . filter not . zipWith (==) xs
----------------------------------------------------------------------------------------------------
hammingDistance1 :: (Eq a) => [a] -> [a] -> Int
hammingDistance1 xs = sum . zipWith compareDNA xs
  where compareDNA x y
          | x == y    = 0
          | otherwise = 1        
----------------------------------------------------------------------------------------------------         
hammingDistance2 :: (Eq a) => [a] -> [a] -> Int
hammingDistance2 xs = length . filter (\(x,y) -> x /= y) . zip xs
----------------------------------------------------------------------------------------------------
hammingDistance3 :: (Eq a) => [a] -> [a] -> Int
hammingDistance3 xs = sum . map (\x -> if fst x == snd x then 0 else 1) . zip xs
                      
                                             
