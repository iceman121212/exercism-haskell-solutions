import Data.List (foldl', sort)
import Data.Map (Map, insertWith, fromList, toList) 

studentList = [ (2,"Aimiee")
               ,(2, "Blair")
               ,(2, "James")
               ,(2, "Paul")
               ,(3, "Chelsea")
               ,(7, "Logan")
               ,(5, "Franklin")
               ,(5, "Bradley")
               ,(1, "Jeff")]


gradeWithStudents :: Int -> [String] -> School
gradeWithStudents g  = schoolFromList . zip (repeat g)
                       
schoolFromList :: [(Int, String)] -> School
schoolFromList = foldl' (flip $ uncurry add) empty
----------------------------------------------------------------------------------------------------
type School = Map Int [String]

foldingFunction :: School -> (Int, String) -> School
foldingFunction = flip $ uncurry add

add :: Int -> String -> School -> School
add = curry (uncurry (insertWith sortedAppend) . (\(g,ns) -> (g, [ns])))
      where sortedAppend xs ys = sort $ xs++ys

empty :: School
empty = fromList []

sorted :: Map Int [String] -> [(Int, [String])]
sorted = toList
        
----------------------------------------------------------------------------------------------------              
-- createNewDB :: k -> [v] -> [(k,[v])]
-- createNewDB g ns = map (\(n,st) -> (n,[st])) $ zip (repeat g) ns
                                
-- foldingFunction :: (Ord k) => Map k [v] -> (k, [v]) -> Map k [v]
-- foldingFunction = flip $ uncurry (insertWith (++))

-- emptyMap :: (Ord k) => Map k [v]
-- emptyMap = fromList []
                           
-- schoolFromList :: (Ord k) => [(k, [v])] -> Map k [v]
-- schoolFromList = foldl' foldingFunction emptyMap

-- gradeWithStudents :: (Ord k) => k -> [v] -> Map k [v]
-- gradeWithStudents g  = schoolFromList . (createNewDB g)


