{-# LANGUAGE OverloadedStrings #-}
--module Main (main) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Attributes
import Data.Colour.Names
import Data.Default.Class
import Control.Lens hiding (argument)
import Data.List.Split
import Data.List
import Control.Monad (when)
import System.Exit (exitSuccess)
import Data.Time.LocalTime

import WeightValues(weightValues,mkDate,filterValues)

-- ||| Declaration of datatypes

-- ||| Plotting
chart :: [[Double]] -> String -> [String] -> Bool -> Renderable ()
chart plot_data title_main titles_series borders = toRenderable layout
 where
    weight1 = plot_lines_style . line_color .~ (customColorSeq!!1)
           $ plot_lines_values .~ [ [ (d,v) | (d,v,_) <- weightValues'] ]
           $ plot_lines_title .~ (titles_series!!0)
           $ def

    weight2 = plot_lines_style . line_color .~ (customColorSeq!!2)
           $ plot_lines_values .~ [ [ (d,v) | (d,_,v) <- weightValues'] ]
           $ plot_lines_title .~ (titles_series!!1)
           $ def

    layout = layoutlr_title .~ title_main
           $ layoutlr_plot_background .~ Just (solidFillStyle $ customColorSeq!!0)
           $ layoutlr_left_axis . laxis_override .~ axisGridHide
           $ layoutlr_right_axis . laxis_override .~ axisGridHide
           $ layoutlr_x_axis . laxis_override .~ axisGridHide
           $ layoutlr_plots .~ [Left (toPlot weight1),
                                Right (toPlot weight2)]
           $ layoutlr_grid_last .~ False
           $ def
 
    customColorSeq = [ toAlphaColour (sRGB 253 246 227) -- background color
                     , toAlphaColour (sRGB 0 255 0) -- weight 1
                     , toAlphaColour (sRGB 0 0 255) -- weight 2
          ]

-- ||| Data loading for plot
loadDataFromFile :: FilePath -> IO [Double]
loadDataFromFile file_name = do
    file_data <- parseFileToStringList file_name
    return $ convertListStringToDouble $ parseLinesToStringList file_data

-- | Load, transform and plot the data
loadData :: IO (PickFn ())
loadData = do
    file_data <- loadDataFromFile from_file
    let minimal_plot_data = map removeDateFromList $ map addIdealWeightToList $ convertListToListOfLists file_data
    let plot_data = addMissingMonths minimal_plot_data
    renderableToFile def to_file $ chart plot_data title_main titles_series True
  where
    from_file = "data.dat"
    to_file = "data.png"
    title_main = "Weight vs ideal weight"
    titles_series = ["weight", "ideal"]

-- TODO: fix this, it's no longer prices.
weightValues' :: [(LocalTime,Double,Double)]
weightValues' = filterValues weightValues (mkDate 1 1 2005) (mkDate 31 12 2006)

-- | Return file content as a list of (IO) strings.
parseFileToStringList :: FilePath -> IO [String]
parseFileToStringList filename = do
  my_data <- readFile filename
  return $ lines my_data

-- ||| Data parsing functions
-- | Parses a list of ;-separated string to a list of strings
-- | Example: ["12;10", "15;5"]
-- | gives ["12", "10", "15", "5"]
parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

-- | Splits a ;-separated string into a list
-- | Example: "12;10"
-- | gives ["12", "10"]
parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" $ filter (/=' ') c

-- | Converst list of strings to list of double values
convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

-- | Convert String to Double datatype
-- TODO: use reads?
convertToDouble :: String -> Double
convertToDouble aString = read aString :: Double

-- | Add difference to list
-- | Example: [12, 10]
-- | gives [12, 10, 2]
--addIdealWeightToList :: Num t => [t] -> [t]
addIdealWeightToList [] = []
addIdealWeightToList [x] = [x]
addIdealWeightToList (x:y:[]) = [x] ++ [y] ++ [74.0]
addIdealWeightToList (x:y:xs) = [x] ++ [y] ++ [74.0] ++ (addIdealWeightToList xs)

--getIdealWeight :: Num t
getIdealWeight = 74.0

-- | Remove first element from list
-- | Example: [20141112;82.3;a comment;20141113;82.1;another comment]
-- | gives: [82.3;82.1]
removeDateFromList [] = []
removeDateFromList [x] = []
removeDateFromList (x:y:[]) = [y]
removeDateFromList (x:y:xs) = [y] ++ (removeDateFromList xs)

-- | Remove last element from list
-- | Example: [20141112;82.3;a comment;20141113;82.1another comment]
-- | gives: [20141112;82.3;20141113;82.1]
removeCommentFromList [] = []
removeCommentFromList [x] = []
removeCommentFromList (x:y:[]) = [x] ++ [y]
removeCommentFromList (x:y:xs) = [x] ++ [y] ++ (removeDateFromList xs)

-- | Turn list into list of lists (2 pairs)
-- | Example: ["12", "10", "15", 5"]
-- | gives [["12", "10"], ["15", 5"]]
convertListToListOfLists :: [a] -> [[a]]
convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

-- | Add missing months
-- | Example: [[12, 10, 2], [15, 5, 10]]
-- | gives [[12, 10, 2], [15, 5, 10], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
addMissingMonths :: Num t => [[t]] -> [[t]]
addMissingMonths [] = getMissingMonthsEmpty 12
addMissingMonths [x] = [x] ++ getMissingMonthsEmpty 11
addMissingMonths (x:y:[]) = [x] ++ [y] ++ getMissingMonthsEmpty 10
addMissingMonths (x:y:xs) = [x] ++ [y] ++ xs ++ getMissingMonthsEmpty (10 - length xs)

-- | Returns a list of [0,0,0] elements for <missing_months> elements
getMissingMonthsEmpty :: Num t => Int -> [[t]]
getMissingMonthsEmpty missing_months = take missing_months $ repeat [0,0,0]

-- ||| General functions
-- | Drop the last n elements from a list
dropLastN :: Int -> [a] -> [a]
dropLastN n xs = reverse $ foldl' (const . drop 1) (reverse xs) (drop n xs)

-- | Get the last n elements from a list
getLastN :: Int -> [a] -> [a]
getLastN n xs = foldl' (const . drop 1) xs (drop n xs)

-- ||| Main
main :: IO (PickFn ())
main = do
    loadData
    exitSuccess
