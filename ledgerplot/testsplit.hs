module Main (main) where

import Control.Monad (liftM)
import Data.List.Split

parseLines [] = []
parseLines [x] = parseCurrent [x]
parseLines (x:xs) = parseCurrent [x] ++ parseLines xs
        
parseCurrent c = splitOn ";" c

--parseFile :: FilePath -> IO [Double]
parseFile filename = do
   myData <- liftM lines . readFile $ filename
   --return myData
   return parseLines myData

main = do
   parseFile "test.dat"
