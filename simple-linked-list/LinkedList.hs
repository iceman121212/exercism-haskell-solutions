-- Here's my first crack at it.  I'll admit
-- that the implementation of
-- `reverseLinkedList` (see last two
-- functions) isn't great: the recursive
-- helper function `revList` isn't tail
-- recursive so this is probably pretty
-- inefficient; maybe one of you Haskell
-- wizards has a better idea


module LinkedList (  nil, isNil, datum, next
                   , new, toList, fromList
                   , reverseLinkedList) where

data LinkedList a = EmptyList | Node a (LinkedList a)
                                                               
nil :: LinkedList a
nil = EmptyList

isNil ::  LinkedList a -> Bool
isNil EmptyList = True
isNil _ = False

datum :: LinkedList a -> a
datum EmptyList = error "Empty LinkedList"
datum (Node x _) = x

next :: LinkedList a -> LinkedList a
next EmptyList = error "Empty LinkedList"
next  (Node _ xs) = xs
                     
new :: a -> LinkedList a -> LinkedList a
new x EmptyList = Node x EmptyList
new x xs = (Node x xs)

toList :: LinkedList a -> [a]
toList EmptyList = []
toList (Node x xs) = x : toList xs

fromList :: [a] -> LinkedList a
fromList xs = foldr new EmptyList xs

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList xs = revList xs EmptyList
                       
revList :: LinkedList a -> LinkedList a -> LinkedList a
revList EmptyList rs = rs
revList (Node x xs) rs = revList xs $ new x rs
