module School (School, add, empty, sorted, grade) where

{-
Just for fun, here's a version that
replaces the list of strings in `School`
(that's associated with each grade number)
with a Set instead.  The benefit is that
we no longer have to write code for
sorting this list (Sets are sorted
by definition).

Also, this version uses the aliases
`GradeNum` and `Name` to improve
readability.
-}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set, union)
import Data.Map (Map, insertWith, findWithDefault)

type GradeNum = Int
type Name = String

newtype School = School (Map GradeNum (Set Name))
                                                    
toMap :: School -> Map GradeNum (Set Name)
toMap (School x) = x

fromMap :: Map GradeNum (Set Name) -> School
fromMap x = School x
                                            
add :: GradeNum -> Name -> School -> School
add k v = fromMap . insertWith union k (S.fromList [v]) . toMap
          
empty :: School
empty = School(M.fromList [])

sorted :: School -> [(GradeNum, [Name])]
sorted = M.toList . fmap S.toList . toMap

grade :: GradeNum -> School -> [Name]
grade g = S.toList . findWithDefault (S.fromList []) g . toMap
