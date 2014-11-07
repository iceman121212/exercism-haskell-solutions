module Grains (square, total) where

import Data.List (foldr, foldl')

{-
Updates:
Have added type signature for the
toggle function `total`; also, based
on discussion in the other exercises,
I've taken out the foldr and foldl
as foldl' outperforms them both

I've only just started compiling with
`-Wall` (thanks to etrepum for pointing
that out) and it's opened my eyes to
problems I wasn't previously aware of.

One thing that I'm not sure how to resolve is
the warning below.  I get one of these warnings
for each test.  Changing the type from
`total :: Integer` to `total :: (Integral a) => a`
didn't help.  I suspect it might be because
the test casts to an Integer type but I'm
not sure.

grains_test.hs:23:11: Warning:
    Defaulting the following constraint(s) to type `Integer'
      (Integral a0) arising from a use of `i' at grains_test.hs:23:11
      (Num a0) arising from the literal `1' at grains_test.hs:23:21
    In the second argument of `(@=?)', namely `i (square 1)'
    In the second argument of `($)', namely `1 @=? i (square 1)'
    In the expression: testCase "square 1" $ 1 @=? i (square 1)
-}

square :: (Integral a) => a -> a
square n = 2 ^ (n - 1)

--toggle:
total :: (Integral a) => a
total = total2;

----------------------------------------------------------------------------------------------------  
total1 :: (Integral a) => a
total1 = foldl' (\acc x -> acc + square x) 0 [1..64]
----------------------------------------------------------------------------------------------------
total2 :: (Integral a) => a
total2 = (sum . map square) [1..64]
                
