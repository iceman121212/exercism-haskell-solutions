module Accumulate (accumulate) where

import Data.List(foldr)

--toggle
accumulate = accumulate3
--toggle
  
accumulate1 :: (a -> b) -> [a] -> [b]
accumulate1 f = foldr ((:).f) []

accumulate2 :: (a -> b) -> [a] -> [b]
accumulate2 _ [] = []
accumulate2 f (x:xs) = [f x] ++ accumulate2 f xs

accumulate3 :: (a -> b) -> [a] -> [b]
accumulate3 _ [] = []
accumulate3 f ys = (accumulate3 f . init) ys ++ [(f. last) ys]

accumulate4 :: (a -> b) -> [a] -> [b]
accumulate4 = map                       
