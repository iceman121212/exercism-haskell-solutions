module Roster (Roster(..), toMap, fromMap, add) where

import Data.Map (Map, insertWith)
import Data.List (sort)
newtype Roster = Roster (Map Int [String]) deriving (Show)

toMap :: Roster -> Map Int [String]
toMap (Roster x) = x

fromMap :: Map Int [String] -> Roster
fromMap x = Roster x

add :: Int -> String -> Roster -> Roster
add k v = fromMap . insertWith sortedAppend k [v] . toMap
  where sortedAppend xs ys = sort $ xs ++ ys
        
