module School (School, add, empty, sorted, grade) where

import Data.List (sort)
import Data.Map (Map, insertWith, fromList,
                 toList, findWithDefault)
 
newtype School = School (Map Int [String])

add :: Int -> String -> School -> School
add k v = insertWith sortedAppend k [v]
  where sortedAppend xs ys = sort $ xs ++ ys
      
empty :: School
empty = fromList []

sorted :: School -> [(Int, [String])]
sorted = toList

grade :: Int -> School -> [String]
grade = findWithDefault []
