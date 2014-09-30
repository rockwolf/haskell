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

main = do
    parseFileToStringList "testdata.dat"
