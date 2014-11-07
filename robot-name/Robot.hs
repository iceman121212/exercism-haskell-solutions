{-# LANGUAGE NoMonomorphismRestriction #-}

module Robot (robotName, mkRobot, resetName) where

import Data.Map (fromList, findWithDefault)
import System.Random (Random, randomRIO)          

data Robot = Robot (IO String)

resetName :: Robot -> IO Robot
resetName (Robot _) = return $ (Robot mkRobotName)

mkRobot :: IO Robot 
mkRobot = return (Robot mkRobotName)

robotName :: Robot -> IO String
robotName (Robot name) = name
                                      
mkRobotName :: IO String
mkRobotName = getRandLetter >>=
              \l1 -> getRandLetter >>=
                     \l2 -> getRandNumber >>=
                            \n1 -> getRandNumber >>=
                                   \n2 -> getRandNumber >>=
                                          \n3 -> return (l1:l2:n1:n2:n3:[])
                                                 
getRandNumber :: IO Char
getRandNumber = randomRIO (1,10) >>= \i -> return $ getNumber i

getNumber :: Int -> Char
getNumber i = findWithDefault ' ' i alphabet
  where alphabet = fromList $ zip [1..] ['0'..'9']
                  
getRandLetter :: IO Char
getRandLetter = randomRIO (1,26) >>= \i -> return $ getLetter i

getLetter :: Int -> Char
getLetter i = findWithDefault ' ' i alphabet
  where alphabet = fromList $ zip [1..] ['A'..'Z']
