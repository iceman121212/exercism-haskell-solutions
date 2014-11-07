{-
I think this solution is more
readabile than my last version;
I suspect it might also be more
idiomatic given it uses the
`foldrWithKey` function.

But it's still not that concise...

I chose to write `invert` separately
(i.e., instead of including it
in a `where` statement within
`transform`) because I felt that it
was complex enough to warrant
having a type signature.
-}

module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, foldrWithKey)

type Score = Int
type Tile  = [Char]

transform :: Map Score [Tile] -> Map Tile Score
transform = fromList . foldrWithKey invert []

invert :: Score -> [Tile] -> [(Tile, Score)] -> [(Tile, Score)]
invert k v acc = zip (clean v) (repeat k) ++ acc
  where clean = map (map toLower)
