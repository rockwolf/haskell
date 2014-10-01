module Main (main) where

import Control.Monad (liftM)
import Data.List.Split

-- | Data parsing functions
parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" c

-- | Conversion functions
convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

parseFileToStringList :: FilePath -> IO [String]
parseFileToStringList filename = do
  my_data <- readFile filename
  return (lines my_data)

-- | Add difference to list
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

-- | Turn list into list of lists
convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

-- TODO: use reads?
convertToDouble aString = read aString :: Double

loadDataFromFile :: FilePath -> [[Double]]
loadDataFromFile file_name = do
    -- the below line gives: [[String]] expected, but IO [String] given?
    file_data <- (parseFileToStringList file_name)
    let chart_data = convertListStringToDouble file_data 
    return chart_data

main = do
    parseFileToStringList "testdata.dat"

--TODO: need to make the result a list of Double, so I can add it to addDifferenceToList
