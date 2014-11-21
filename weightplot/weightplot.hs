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
-- TODO: some of these imports might no longer be necessary... perform cleanup.

import WeightValues(weightValues,mkDate,filterValues)
import DataConversion(convertListToListOfLists, splitLinesToElements, removeFirstFromGroupedList)
import FileIO(loadFileToStringList)

-- ||| Declaration of datatypes
let ideal_weight = 74.0

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
    file_data <- loadFileToStringList file_name
    return $ splitLinesToElements file_data

-- | Load, transform and plot the data
loadData :: IO (PickFn ())
loadData = do
    file_data <- loadDataFromFile from_file
    let minimal_plot_data = convertListToListOfLists $ map addIdealWeightToGroupedList $ map removeFirstFromGroupedList $ file_data
    --let plot_data = addMissingMonths minimal_plot_data
    let plot_data = minimal_plot_data
    renderableToFile def to_file $ chart plot_data title_main titles_series True
  where
    from_file = "data.dat"
    to_file = "data.png"
    title_main = "Weight vs ideal weight"
    titles_series = ["weight", "ideal"]

-- TODO: weightValues gets the data from the hardcoded data.
-- Make the loadData function and create a new function that
-- transforms the data into the same format as in WeighValues
-- and than we can use that.
weightValues' :: [(LocalTime,Double,Double)]
weightValues' = filterValues weightValues (mkDate 1 1 2005) (mkDate 31 12 2006)

-- ||| Data parsing functions
-- | Add number to list
-- | Example: [12, 10]
-- | gives [12, 10, <number>]
--addIdealWeightToGroupedList :: Num t => [t] -> [t]
-- TODO: make ideal_weight a parameter and call it with the parameter, to make this function more generic.
addIdealWeightToGroupedList [] = []
addIdealWeightToGroupedList [x] = [x]
addIdealWeightToGroupedList (x:y:[]) = [x] ++ [y] ++ [ideal_weight]
addIdealWeightToGroupedList (x:y:xs) = [x] ++ [y] ++ [ideal_weight] ++ (addIdealWeightToGroupedList xs)

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

-- ||| Main
main :: IO (PickFn ())
main = do
    loadData
    exitSuccess
