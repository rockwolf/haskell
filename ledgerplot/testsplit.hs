module Main (main) where

import Control.Monad (liftM)
import Data.List.Split

-- ||| Data parsing/conversing functions
parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" c

convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

-- ||| Data loading functions
-- TODO: use reads?
convertToDouble aString = read aString :: Double

parseFileToStringList :: FilePath -> IO [String]
parseFileToStringList filename = do
  my_data <- readFile filename
  return (lines my_data)

loadDataFromFile :: FilePath -> IO [Double]
loadDataFromFile file_name = do
    file_data <- parseFileToStringList file_name
    let chart_data = convertListStringToDouble $ parseLinesToStringList file_data
    return chart_data

-- ||| Main
main = do
    parseFileToStringList "testdata.dat"

--TODO: need to make the result a list of Double, so I can add it to addDifferenceToList
