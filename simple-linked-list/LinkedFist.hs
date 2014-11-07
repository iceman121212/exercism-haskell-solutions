module LinkedFist (nil, isNil, datum, next, new) where

data LinkedFist a = EmptyList | Node a (LinkedFist a) deriving (Show, Eq)

nil :: LinkedFist a
nil = EmptyList

isNil :: (Eq a) => LinkedFist a -> Bool
isNil EmptyList = (list == EmptyList)
