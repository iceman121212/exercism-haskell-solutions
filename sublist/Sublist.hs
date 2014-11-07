module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))
import Data.List (foldl', isInfixOf)
import qualified Data.Foldable as F

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

--As reference, my subumitted results are based on Algorithm 2A but I've included
--a couple different versions below (further explanation below).

--Each algorithm consists of two parts: a `sublist`function (which is called by
--`sublist_test.hs`) and a helper `isSublist` function.  Below are three very
--different implementations of `isSublist` (A, B and C) and two slightly different
--implementations of `sublist` (i.e., 1 and 2).
                                                              
--Also, in order to measure algorithmic efficiency, sublist_test ought to have an
--additional test in which a very short sublist matches the end of a very long
--reference sequence, e.g., sublist [99997..100000] [1..100000].  It doesn't
--make a difference if the `isInfixOf` function is used; however, the difference
--in execution time can vary greatly among methodologies that don't use
--the `isInfixOf` operator.  For exapmle, Algorithm 1B took 0.29 seconds to
--complete the additional test; Algorithm 1C took over 25 seconds.  (Algorithm 1A,
--i.e., the implementation which uses `isInfixOf` took only ~0.02 seconds.)
 
sublist   cs rs = sublist1   cs rs
isSublist cs rs = isSublistB cs rs

--The two lines above serve as a `toggle` for switching between the various
--implementations.  It's currently set to 2A.  If we were to change it to 2B,
--it would look like this:
--
--    sublist   cs rs = sublist2   cs rs
--    isSublist cs rs = isSublistB cs rs

----------------------------------------------------------------------------------------------------
--ALGORITHM 1:

sublist1 :: (Eq a) => [a] -> [a] -> Sublist
sublist1 [] [] = Equal
sublist1 [] _ = Sublist
sublist1 _ [] = Superlist
sublist1 cs rs
 | cs == rs = Equal
 | length cs <  length rs = testSublist cs rs Sublist
 | otherwise = testSublist rs cs Superlist
  where testSublist cs rs sublistType = if isSublist cs rs then sublistType else Unequal
        
----------------------------------------------------------------------------------------------------
--ALGORITHM 2:
                  
sublist2 :: (Eq a) => [a] -> [a] -> Sublist
sublist2 [] [] = Equal
sublist2 [] _ = Sublist
sublist2 _ [] = Superlist
sublist2 cs rs
  | cs == rs = Equal
  | isSublist cs rs = Sublist
  | isSublist rs cs = Superlist
  | otherwise = Unequal
                   
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--All of the following times assume `sublist` function 1 (i.e., the times
--below are for Algorithms 1A, 1B and 1C).  

--ALGORITHM A (the idiomatic way, i presume)        

--time to complete sublist_test:
--real    0m0.903s
--user    0m0.555s
--sys     0m0.338s
        
--time to complete additional test:
--0.01 secs (as per GHCi)
        

isSublistA :: (Eq a) => [a] -> [a] -> Bool
isSublistA cs rs = isInfixOf cs rs

----------------------------------------------------------------------------------------------------
--ALGORITHM B:

--time to complete sublist_test:
--real    0m5.374s
--user    0m3.846s
--sys     0m1.563s

--time to complete additional test:
--0.29 secs (as per GHCi)

isSublistB :: (Eq a) => [a] -> [a] -> Bool                                     
isSublistB cs rs = let rs_Seq = Seq.fromList rs
                       cs_Seq = Seq.fromList cs
                   in  snd $ F.foldl'(\(oldAcc,bool) r ->
                                       let n = Seq.length cs_Seq
                                           buildUpComplete = Seq.length oldAcc == n
                                           newAcc = if buildUpComplete then
                                                      Seq.drop 1 oldAcc |> r
                                                    else
                                                      oldAcc |> r
                                       in if bool == True || newAcc == cs_Seq then
                                            (oldAcc, True)
                                          else
                                            (newAcc, bool)                                        
                                     ) (Seq.fromList [], False) rs_Seq                     

----------------------------------------------------------------------------------------------------
--ALGORITHM C:

--time to complete sublist_test:
--real    0m1.084s
--user    0m0.817s
--sys     0m0.257s
        
--time to complete additional test:
--25.31 secs (as per GHCi)
        
isSublistC :: (Eq a) => [a] -> [a] -> Bool                                     
isSublistC cs (r:rs)
 | length (r:rs) < n = False
 | cs == take n (r:rs) = True
 | otherwise = isSublistC cs rs
  where n = length cs        
        
