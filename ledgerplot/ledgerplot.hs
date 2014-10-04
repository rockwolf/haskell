module Main (main) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Attributes
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.List.Split
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command,
    argument, longOption)

data PlotType = IncomeVsExpenses | Networth deriving (Show, Eq)

-- ||| Plotting
chart :: PlotType -> [[Double]] -> String -> Bool -> Renderable ()
chart plot_type plot_data title_main borders = toRenderable layout
 where
  layout = 
        layout_title .~ title_main ++ " " ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars plotData ]
      $ def :: Layout PlotIndex Double

  plotData = plot_bars_titles .~ ["Expenses","Income","P/L"]
      $ plot_bars_values .~ addIndexes plot_data
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle customColorSeq)
      $ def

  alabels = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

  customColorSeq = [ toAlphaColour (sRGB 255 0 0)
                     , toAlphaColour (sRGB 0 255 0)
                     , toAlphaColour (sRGB 0 0 255)
          ]
  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)

-- ||| Data loading for plot
--loadData :: PlotType -> [[Double]]
--loadData plot_type
----    | pt == IncomeVsExpenses = map addDifferenceToList [[20,45],[45,30],[30,20],[70,25],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45]]
--    | plot_type == IncomeVsExpenses = map addDifferenceToList $ loadDataFromFile "testdata.dat"
--    | otherwise = [[0]]

-- ||| IO related functions
loadDataFromFile :: FilePath -> IO [Double]
loadDataFromFile file_name = do
    file_data <- parseFileToStringList file_name
    let chart_data = convertListStringToDouble $ parseLinesToStringList file_data
    return chart_data

-- | Load, transform and plot the data for the given PlotType
loadData :: t -> IO (PickFn ())
loadData plot_type = do
    file_data <- loadDataFromFile $ fromFileName IncomeVsExpenses
    let minimal_plot_data = map addDifferenceToList $ convertListToListOfLists file_data
    let plot_data = addMissingMonths minimal_plot_data
    renderableToFile def "income_vs_expenses.png" $ chart IncomeVsExpenses plot_data "Income vs expenses" True

-- | Get the correct filename for the given PlotType
fromFileName :: PlotType -> String
fromFileName plot_type
    | plot_type == IncomeVsExpenses = "testdata.dat" 
    | otherwise = "testdata.dat"

parseFileToStringList :: FilePath -> IO [String]
parseFileToStringList filename = do
  my_data <- readFile filename
  return (lines my_data)

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
parseCurrent c = splitOn ";" c

convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

-- TODO: use reads?
convertToDouble :: String -> Double
convertToDouble aString = read aString :: Double

-- | Add difference to list
-- | Example: [12, 10]
-- | gives [12, 10, 2]
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

-- | Turn list into list of lists (2 pairs)
-- | Example: ["12", "10", "15", 5"]
-- | gives [["12", "10"], ["15", 5"]]
convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

-- | Add missing months
-- | Example: [[12, 10, 2], [15, 5, 10]]
-- | gives [[12, 10, 2], [15, 5, 10], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]
addMissingMonths [] = getMissingMonthsEmpty 12
addMissingMonths [x] = [x] ++ getMissingMonthsEmpty 11
addMissingMonths (x:y:[]) = [x] ++ [y] ++ getMissingMonthsEmpty 10
addMissingMonths (x:y:xs) = [x] ++ [y] ++ xs ++ getMissingMonthsEmpty (10 - length xs)

-- | Returns a list of [0,0,0] elements for <missing_months> elements
getMissingMonthsEmpty missing_months = take missing_months $ repeat [0,0,0]

-- ||| Main
main :: IO (PickFn ())
main = do
    let plot_type = IncomeVsExpenses
    loadData plot_type
