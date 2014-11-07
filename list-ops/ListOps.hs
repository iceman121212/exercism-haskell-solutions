-- foldl' and foldr were particularly
-- instructive.  It was interesting to learn
-- about how `seq` works.  I'm not sure
-- about my `foldr` implementation as
-- I think it might just be a perverse
-- version of foldl :) (but i'm not sure)

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map
  , filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc [x]    = f acc x
foldl' f acc (x:xs) = seq acc' $ foldl' f acc' xs
  where acc' = f acc x
                      
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc [x]    = f x acc
foldr f acc (x:xs) = f x $ foldr f acc xs

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse []     = []
reverse [x]    = [x]
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f [x]    = [f x]
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [y | y <- xs, f y]

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) [x]    ys = x:ys
(++) (x:xs) ys = (x:) $ (++) xs ys

concat :: [[a]] -> [a]
concat []     = []
concat [x]    = x
concat (x:xs) = x ++ concat xs
