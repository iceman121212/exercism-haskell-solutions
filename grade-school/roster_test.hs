import Roster
import Data.Map (fromList)
import Data.List (foldl')
  
-- addToNewRoster :: Int -> String -> Roster
-- addToNewRoster g 

example :: Roster
example = (fromMap . fromList) [(3,["Dipak", "Andrew", "Keone"])]

addToRoster :: Roster -> [(Int, String)] -> Roster
addToRoster roster xs = foldl' (flip $ uncurry add) roster xs

addition :: [(Int, String)]
addition = [(1,"Frodo"), (2, "Sam"), (3, "Gandalf")]

main :: IO ()
main = putStrLn $ show $ addToRoster example addition
