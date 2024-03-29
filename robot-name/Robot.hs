{-# LANGUAGE NoMonomorphismRestriction #-}

module Robot (robotName, mkRobot, resetName) where

import Data.Map (fromList, findWithDefault)
import System.Random (Random, randomRIO)
import Control.Monad (replicateM)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)

newtype Robot = Robot { name :: String }

mkRobot :: IO (IORef Robot)
mkRobot = mkRobotName >>= return . Robot >>= newIORef

robotName :: IORef Robot -> IO String
robotName rr = readIORef rr >>= return . name

resetName :: IORef Robot -> IO ()
resetName rr = mkRobotName >>=
               \newName -> modifyIORef rr (\r -> r {name = newName})

mkRobotName :: IO String
mkRobotName = replicateM 2 getRandLetter >>=
              \l -> replicateM 3 getRandNumber >>=
                    \n -> return $ l ++ n

getRandNumber :: IO Char                          
getRandNumber = fmap getNumber $ randomRIO (1, 10)

getRandLetter :: IO Char
getRandLetter = fmap getLetter $ randomRIO (1, 26)

getNumber :: Int -> Char
getNumber i = findWithDefault ' ' i alphabet
  where alphabet = fromList $ zip [1..] ['0'..'9']

getLetter :: Int -> Char
getLetter i = findWithDefault ' ' i alphabet
  where alphabet = fromList $ zip [1..] ['A'..'Z']
