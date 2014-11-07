{-
Frankly, I'm not too happy with my solution
as it's pretty clunky.  I suspect there's a
much simpler way using a monad?

I thought `fmap` might be useful for operating
directly on a map but it turns out that `fmap`
doesn't let you modify keys.

Planning to resubmit with a better version shortly.
-}

module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, toList)

type Score = Int
type Tile  = [Char]

transform :: Map Score [Tile] -> Map Tile Score
transform = fromList . concatMap (uncurry invert) . toList
  where
    invert :: Score -> [Tile] -> [(Tile,Score)]
    invert s = map (\t -> ([toLower t],s)) . concat 
               
