{-# LANGUAGE OverloadedStrings #-}
--module Main (main) where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
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

import DataConversion(splitLinesToListOfStrings, dropLastN)
import FileIO(loadFileToStringList)
import DateUtil(mkDate)

-----------------------------------------------------------------------------
-- ||| Declaration of datatypes
-----------------------------------------------------------------------------
c_ideal_weight = 74.0

-----------------------------------------------------------------------------
-- ||| Plotting
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | plotLines2 
-- | Create a line chart with 2 lines
-----------------------------------------------------------------------------
-- TODO: make it [[(localtime,double,double)]]
-- so the second weight can be added too
plotLines2 :: [[(LocalTime,Double,Double)]] -> String -> [String] -> Renderable ()
plotLines2 a_plot_data a_title_main a_titles_series = toRenderable layout
  where
    weight1 = plot_lines_style . line_color .~ (customColorSeq!!2)
           $ plot_lines_values .~ [ [ (d, v) | (d,v,_) <- a_plot_data!!0] ]
           $ plot_lines_title .~ (a_titles_series!!0)
           $ def

    weight2 = plot_lines_style . line_color .~ (customColorSeq!!1)
           $ plot_lines_values .~ [ [ (d, v) | (d,_,v) <- a_plot_data!!0] ]
           $ plot_lines_title .~ (a_titles_series!!1)
           $ def

    layout = layout_title .~ a_title_main
           $ layout_plot_background .~ Just (solidFillStyle $ customColorSeq!!3)
           $ layout_background .~ (solidFillStyle $ customColorSeq!!3)
           $ layout_foreground .~ customColorSeq!!0
           $ layout_x_axis . laxis_override .~ axisGridHide
           $ layout_plots .~ [ (toPlot weight1),
                                (toPlot weight2)]
           $ def

--    layout = layoutlr_title .~ title_main
--           $ layoutlr_plot_background .~ Just (solidFillStyle $ customColorSeq!!0)
--           $ layoutlr_left_axis . laxis_override .~ axisGridHide
--           $ layoutlr_right_axis . laxis_override .~ axisGridHide
--           $ layoutlr_x_axis . laxis_override .~ axisGridHide
--           $ layoutlr_plots .~ [ Left (toPlot weight1),
--                                Right (toPlot weight2)]
--           $ layoutlr_grid_last .~ False
--           $ def

     
    customColorSeq = [ toAlphaColour (sRGB24 253 246 227) -- base0
                     , toAlphaColour (sRGB24 133 153 0) -- green
                     , toAlphaColour (sRGB24 38 139 210) -- blue
                     , toAlphaColour (sRGB24 0 43 54) -- base03
                     , toAlphaColour (sRGB24 0 0 0) -- black
          ]

-----------------------------------------------------------------------------
-- ||| Data loading for plot
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | loadDataFromFile
-- | Read the data from the file and put it in a list
-----------------------------------------------------------------------------
--loadDataFromFile :: FilePath -> IO [Double]
loadDataFromFile a_file_name = do --[0.0, 0.1]
    l_file_data <- loadFileToStringList a_file_name
    -- l_file_data should be ["date1;v1a;v1b;comment1", "date2;v2a;v2b;comment2"]
    let l_split_data = splitLinesToListOfStrings ";" l_file_data
    -- l_split_data should be [["date1", "v1a", "v1b", "comment1"], ["date2", "v2a", "v2b", "comment2"]]
    les l_split_data_no_comment = map  (dropLastN 1) l_split_data
    -- l_split_data_no_comment should be [["date1", "v1a", "v1b"], ["date2", "v2a", "v2b"]]
    --let l_only_doubles_as_string = map removeLastFromGroupedList $ map removeFirstFromGroupedList $ l_split_data
    -- l_only_doubles_as_string should be ["v1a", "v1b", "v2a", "v2b"]

-----------------------------------------------------------------------------
-- | loadData
-- | Load, transform and plot the data
-----------------------------------------------------------------------------
loadData :: IO (PickFn ())
loadData = do
    -- TODO:
    -- load data from file ["date;v1;comment", "date;v3;comment"]
    -- transform data to list of elements ["date", v1, v2, "comment", "date", v3, v4, "comment"]
    -- remove each comment field ["date", v1, "date", v2,]
    -- add ideal weight ["date", v1, vi, "date", v2, vi]
    l_file_data <- loadDataFromFile from_file
    --let l_minimal_plot_data = convertListToListOfLists $ map addIdealWeightToGroupedList $ map removeLastFromGroupedList $ map removeFirstFromGroupedList $ file_data
    --let l_minimal_plot_data = convertListToListOfLists $ map addIdealWeightToGroupedList $ map removeLastFromGroupedList $ map removeFirstFromGroupedList $ file_data
    --let plot_data = addMissingMonths minimal_plot_data
    --let plot_data = minimal_plot_data
    --renderableToFile def to_file $ plotLines2 plot_data title_main titles_series True
    renderableToFile def l_to_file $ plotLines2 l_plot_data l_title_main l_titles_series
  where
    l_from_file = "data.dat"
    l_to_file = "data.png"
    l_title_main = "Weight vs ideal weight"
    l_titles_series = ["weight", "ideal"]
    l_plot_data = [filterValues (weightValues values) (mkDate 1 1 2014) (mkDate 31 12 2014)]

-----------------------------------------------------------------------------
-- | weightValues'
-- | Transform the dates and filter the data.
-----------------------------------------------------------------------------
--weightValues' :: [(LocalTime,Double,Double)]
--weightValues' day_start month_start year_start day_end month_end year_end = filterValues weightValues (
--    mkDate day_start month_start year_start) (mkDate day_end month_end year_end)

-----------------------------------------------------------------------------
-- | weightValues
-- | Create plotable data from available sets
--
-- Example: [(10, 12, 2014, 80.1), (11, 12, 2014, 79.6)]
-----------------------------------------------------------------------------
weightValues :: [(Int, Int, Int, Double, Double)] -> [(LocalTime,Double,Double)]
weightValues a_values = [ (mkDate dd mm yyyy, p1, p2) | (yyyy, mm, dd, p1, p2) <- a_values ]

-- example values
values = [(2014, 01, 01, 82.4, 72.4),
              (2014, 01, 08, 81.3, 72.4),
              (2014, 01, 15, 80.1, 72.4),
              (2014, 01, 22, 79.4, 72.4),
              (2014, 01, 29, 79.5, 72.4),
              (2014, 02, 05, 78.9, 72.4),
              (2014, 02, 12, 79.7, 72.4),
              (2014, 02, 17, 79.2, 72.4),
              (2015, 01, 01, 81.4, 73.4),
              (2015, 01, 08, 80.3, 73.4),
              (2015, 01, 15, 79.1, 73.4),
              (2015, 01, 22, 78.4, 73.4),
              (2015, 01, 29, 79.2, 73.4),
              (2015, 02, 05, 80.9, 73.4),
              (2015, 02, 12, 79.1, 73.4),
              (2015, 02, 17, 78.5, 73.4)]

-----------------------------------------------------------------------------
-- | filterValues
-- | Apply date filter to data.
-----------------------------------------------------------------------------
filterValues a_weight_values a_t1 a_t2 = [ v | v@(d,_,_) <- a_weight_values
                                   , let t = d in t >= a_t1 && t <= a_t2]

-----------------------------------------------------------------------------
-- ||| Data parsing functions
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Add number to list
--
-- Example: [12, 10]
-- gives [12, 10, <number>]
-----------------------------------------------------------------------------
--addIdealWeightToGroupedList :: Num t => [t] -> [t]
-- TODO: make ideal_weight a parameter and call it with the parameter, to make this function more generic.
addIdealWeightToGroupedList [] = []
addIdealWeightToGroupedList [x] = [x]
addIdealWeightToGroupedList (x:y:[]) = [x] ++ [y] ++ [c_ideal_weight]
addIdealWeightToGroupedList (x:y:xs) = [x] ++ [y] ++ [c_ideal_weight] ++ (addIdealWeightToGroupedList xs)

-----------------------------------------------------------------------------
-- | addMissingMonths
-- | Add missing months to get a complete calendar year.
--
-- Example: [[12, 10, 2], [15, 5, 10]]
-- gives [[12, 10, 2], [15, 5, 10], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
-----------------------------------------------------------------------------
addMissingMonths :: Num t => [[t]] -> [[t]]
addMissingMonths [] = getMissingMonthsEmpty 12
addMissingMonths [x] = [x] ++ getMissingMonthsEmpty 11
addMissingMonths (x:y:[]) = [x] ++ [y] ++ getMissingMonthsEmpty 10
addMissingMonths (x:y:xs) = [x] ++ [y] ++ xs ++ getMissingMonthsEmpty (10 - length xs)

-----------------------------------------------------------------------------
-- | getMissingMonthsEmpty
-- | Returns a list of [0,0,0] elements for <missing_months> elements
-----------------------------------------------------------------------------
getMissingMonthsEmpty :: Num t => Int -> [[t]]
getMissingMonthsEmpty a_missing_months = take a_missing_months $ repeat [0,0,0]

-----------------------------------------------------------------------------
-- ||| Main
-----------------------------------------------------------------------------
main :: IO (PickFn ())
main = do
    putStrLn "Loading data and creating png..."
    loadData
    putStrLn "Done."
    exitSuccess
