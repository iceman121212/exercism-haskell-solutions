module Gigasecond (fromDay) where

import Data.Time.Calendar

{-
Updates: Added the typecast for the exponent; I wasn't
even aware of the issue given that I hadn't been
compiling with -Wall.
-}

fromDay :: Day -> Day
fromDay birthday = addDays numDaysInGigasecond birthday
  where numDaysInGigasecond = 10^(9::Int) `div` (60*60*24)
