module Main (main) where

import Control.Monad (liftM)
import Data.List.Split

parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" c

--parseFileToStringList :: FilePath -> IO [String]
--parseFile :: FilePath -> IO [Double]
parseFileToStringList filename = do
  my_data <- readFile filename
  return (lines my_data)

-- | Add difference to list
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

main = do
    addDifferenceToList $ parseFileToStringList "testdata.dat"

--TODO: need to make the result a list of Double, so I can add it to addDifferenceToList
