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

import DataConversion(convertListToListOfLists, splitLinesToElements, removeFirstFromGroupedList, removeLastFromGroupedList)
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
plotLines2 :: [[Double]] -> String -> [String] -> Bool -> Renderable ()
plotLines2 plot_data title_main titles_series borders = toRenderable layout
  where
--    weight1 = plot_lines_style . line_color .~ (customColorSeq!!1)
--           $ plot_lines_values .~ [ [ (d,v) | (d,v,_) <- weightValues'] ]
--           $ plot_lines_title .~ (titles_series!!0)
--           $ def
--
--    weight2 = plot_lines_style . line_color .~ (customColorSeq!!2)
--           $ plot_lines_values .~ [ [ (d,v) | (d,_,v) <- weightValues'] ]
--           $ plot_lines_title .~ (titles_series!!1)
--           $ def
--
    layout = layoutlr_title .~ title_main
           $ layoutlr_plot_background .~ Just (solidFillStyle $ customColorSeq!!0)
           $ layoutlr_left_axis . laxis_override .~ axisGridHide
           $ layoutlr_right_axis . laxis_override .~ axisGridHide
           $ layoutlr_x_axis . laxis_override .~ axisGridHide
--           $ layoutlr_plots .~ [Left (toPlot weight1),
--                                Right (toPlot weight2)]
           $ layoutlr_grid_last .~ False
           $ def
 
    customColorSeq = [ toAlphaColour (sRGB 253 246 227) -- background color
                     , toAlphaColour (sRGB 0 255 0) -- weight 1
                     , toAlphaColour (sRGB 0 0 255) -- weight 2
          ]
--
-----------------------------------------------------------------------------
-- ||| Data loading for plot
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | loadDataFromFile
-- | Read the data from the file and put it in a list
-----------------------------------------------------------------------------
loadDataFromFile :: FilePath -> IO [Double]
loadDataFromFile a_file_name = do
    l_file_data <- loadFileToStringList a_file_name
    -- l_file_data should be ["date1;v1a;v1b;comment1", "date2;v2a;v2b;comment2"] now
    --let l_split_data = splitLinesToElements l_file_data
    -- l_split_data should be ["date1", "v1a", ... , "comment2"] now
    --let l_only_doubles_as_string = map removeLastFromGroupedList $ map removeFirstFromGroupedList $ l_split_data
    -- l_only_doubles_as_string should be ["v1a", "v1b", "v2a", "v2b"]
    return ["0.0", "0.1"]

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
    file_data <- loadDataFromFile from_file
    --let minimal_plot_data = convertListToListOfLists $ map addIdealWeightToGroupedList $ map removeLastFromGroupedList $ map removeFirstFromGroupedList $ file_data
    --let plot_data = addMissingMonths minimal_plot_data
    --let plot_data = minimal_plot_data
    --renderableToFile def to_file $ plotLines2 plot_data title_main titles_series True
    renderableToFile def to_file $ plotLines2 [[81.4,74.0],[78.6,74.0]] title_main titles_series True
  where
    from_file = "data.dat"
    to_file = "data.png"
    title_main = "Weight vs ideal weight"
    titles_series = ["weight", "ideal"]

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
weightValues plot_data_raw = [ (mkDate dd mm yyyy, p1, p2) | (dd,mm,yyyy,p1, p2) <- plot_data_raw ]

-----------------------------------------------------------------------------
-- | filterValues
-- | Apply date filter to data.
-----------------------------------------------------------------------------
filterValues weight_values t1 t2 = [ v | v@(d,_,_) <- weight_values
                                   , let t = d in t >= t1 && t <= t2]

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
getMissingMonthsEmpty missing_months = take missing_months $ repeat [0,0,0]

-----------------------------------------------------------------------------
-- ||| Main
-----------------------------------------------------------------------------
main :: IO (PickFn ())
main = do
    --loadData
    putStrLn "dumbass"
    exitSuccess
