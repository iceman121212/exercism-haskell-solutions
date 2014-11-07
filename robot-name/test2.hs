{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Random (Random, randomRIO)

main = do
  a <- randomRIO
  print a
  a <- randomRIO
  print a
  return a
