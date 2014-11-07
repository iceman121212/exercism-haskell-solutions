module
  Strain (keep, discard) where

import Data.List (foldr)

{-
Four versions included: (1) list comprehension,
(2) foldr, (3) `let`/`in` + recursion
and (4) guards + recursion -}

----------------------------------------------------------------------------------------------------
keep :: (a -> Bool) -> [a] -> [a]
keep    = keep1

discard :: (a -> Bool) -> [a] -> [a]
discard f = keep (not.f)
----------------------------------------------------------------------------------------------------
keep1 :: (a -> Bool) -> [a] -> [a]
keep1 f xs = [y | y <- xs, f y]
             
keep2 :: (a -> Bool) -> [a] -> [a]
keep2 f xs = foldr (\x acc -> if f x then x:acc else acc) [] xs
             
keep3 :: (a -> Bool) -> [a] -> [a]
keep3 _ []     = []
keep3 f (x:xs)
  | f x = x:(keep2 f xs)
  | otherwise = keep2 f xs
                
keep4 :: (a -> Bool) -> [a] -> [a]
keep4 _ [] = []
keep4 f (x:xs) =
  let n = if f x then (x:) else id
  in  n (keep3 f xs)
