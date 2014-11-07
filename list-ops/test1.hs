import Data.List
import Criterion.Main

main :: IO ()
main = do
   -- content <- learquivo "mkList1.txt"  
   let content = [ [big, big - step.. 0] | big <- [1000..1010], step <- [1..5]] :: [[Int]]
   defaultMain
     [ bench "benchmark-name" (nf (map sort) content)]
