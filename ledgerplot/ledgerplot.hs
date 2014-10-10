{-# LANGUAGE OverloadedStrings #-}
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
import Data.List
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command,
    argument, longOption)

-- ||| Declaration of datatypes
data PlotType = IncomeVsExpenses | Networth deriving (Show, Eq)
data PlotPeriod = All | AllMonthly | Year | YearMonthly | Period | PeriodMonthly deriving (Show, Eq)

-- ||| Plotting
chart :: PlotType -> [[Double]] -> String -> [String] -> Bool -> Renderable ()
chart plot_type plot_data title_main titles_series borders = toRenderable layout
 where
  layout = 
        layout_title .~ title_main ++ " " ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars plotData ]
      $ def :: Layout PlotIndex Double

  plotData = plot_bars_titles .~ titles_series
      $ plot_bars_values .~ addIndexes plot_data
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle customColorSeq)
      $ def

  alabels = getLabelsSeries plot_type

  customColorSeq = [ toAlphaColour (sRGB 255 0 0)
                     , toAlphaColour (sRGB 0 255 0)
                     , toAlphaColour (sRGB 0 0 255)
          ]
  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)

-- ||| Data loading for plot
loadDataFromFile :: FilePath -> PlotType -> IO [Double]
loadDataFromFile file_name plot_type = do
    file_data <- parseFileToStringList file_name plot_type
    let chart_data = convertListStringToDouble $ parseLinesToStringList file_data
    return chart_data

-- | Load, transform and plot the data for the given PlotType and PlotPeriod
loadData :: PlotType -> PlotPeriod -> IO (PickFn ())
loadData plot_type plot_period = do
    file_data <- loadDataFromFile from_file plot_type
    let minimal_plot_data = map addDifferenceToList $ convertListToListOfLists file_data
    let plot_data = addMissingMonths minimal_plot_data
    renderableToFile def to_file $ chart plot_type plot_data title_main titles_series True
  where
    from_file = fromFileName plot_type plot_period
    to_file = toFileName plot_type plot_period
    title_main = getTitleMain plot_type plot_period
    titles_series = getTitlesSeries plot_type

-- | Get the correct filename for the given PlotType and PlotPeriod
fromFileName :: PlotType -> PlotPeriod -> String
fromFileName plot_type plot_period
    | (plot_type == IncomeVsExpenses) && (plot_period == All) = "testdata.dat" 
    | otherwise = "testdata.dat"

-- | Get the correct output filename for the given PlotType and PlotPeriod
toFileName :: PlotType -> PlotPeriod -> String
toFileName plot_type plot_period
    | (plot_type == IncomeVsExpenses) && (plot_period == All) = "income_vs_expenses.png"
    | otherwise = "income_vs_expenses.png"

-- | Get the correct main title for the given PlotType and PlotPeriod
getTitleMain :: PlotType -> PlotPeriod -> String
getTitleMain plot_type plot_period
    | (plot_type == IncomeVsExpenses) && (plot_period == All) = "Income vs expenses"
    | otherwise = "Income vs expenses"

-- | Get the correct names for the series in the given PlotType
getTitlesSeries :: PlotType -> [String]
getTitlesSeries plot_type
    | plot_type == IncomeVsExpenses = ["Expenses","Income","P/L"]
    | otherwise = ["Expenses","Income","P/L"]

-- | Get the labels for the series of the given PlotType
getLabelsSeries :: PlotType -> [String]
getLabelsSeries plot_type
    -- TODO: add period/yearly/monthly destinction
    | plot_type == IncomeVsExpenses = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
    | otherwise = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

-- | Return file content as a list of (IO) strings.
parseFileToStringList :: FilePath -> PlotType -> IO [String]
parseFileToStringList filename plot_type = do
  my_data <- readFile filename
  return $ if hasSummary plot_type then dropLastN 2 (lines my_data) else (lines my_data)

-- | Returns boolean that's true if the PlotType has a summary of totals as the last 2 lines
hasSummary :: PlotType -> Boolean
hasSummary plot_type
    | IncomeVsExpenses = True
    | otherwise = False

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
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

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
addMissingMonths :: [[Double]] -> [[Double]]
addMissingMonths [] = getMissingMonthsEmpty 12
addMissingMonths [x] = [x] ++ getMissingMonthsEmpty 11
addMissingMonths (x:y:[]) = [x] ++ [y] ++ getMissingMonthsEmpty 10
addMissingMonths (x:y:xs) = [x] ++ [y] ++ xs ++ getMissingMonthsEmpty (10 - length xs)

-- | Returns a list of [0,0,0] elements for <missing_months> elements
getMissingMonthsEmpty :: Int -> [Double]
getMissingMonthsEmpty missing_months = take missing_months $ repeat [0,0,0]

-- ||| General functions
-- | Drop the last n elements from a list
dropLastN :: Int -> [a] -> [a]
dropLastN n xs = reverse $ foldl' (const . drop 1) (reverse xs) (drop n xs)

-- | Get the last n elements from a list
getLastN :: Int -> [a] -> [a]
getLastN n xs = foldl' (const . drop 1) xs (drop n xs)

-- | Option parsing functions
getPlotPeriodFromOptions :: PlotType -> Boolean -> PlotPeriod
getPlotPeriodFromOptions plot_type plot_detail plot_year 
  | (PlotType == IncomeVsExpenses) = if plot_detail then
-- TODO: find a good way to determine the plotPeriod, based on the input options.  


-- ||| Main
main :: IO (PickFn ())
main = do
    -- TODO: implement command line parameter parsing with docopt
    opts <- optionsWithUsageFileDebug "USAGE.txt"
    print opts
    putStrLn ""
    when (opts `isPresent` (longOption "income-vs-expenses")) $ do
        let plot_type = IncomeVsExpenses
    --when (opts `isPresent` (longOption "networth")) $ do
    --    let plot_type = Networth
    when (opts `isPresent` (longOption "year")) $ do
        plot_year <- opts `getArg` (argument "year")
        putStrLn " --year="
    when (opts `isPresent` (longOption "start-date")) $ do
        plot_start_date <- opts `getArg` (argument "start-date")
        putStrLn " --start-date"
    when (opts `isPresent` (longOption "end-date")) $ do
        plot_end_date <- opts `getArg` (argument "end-date")
        putStrLn " --end-date"
    when (opts `isPresent` (longOption "detail")) $ do
        let plot_detail = True
        putStrLn " --detail"
    
    putStrLn $ " --start-date=" ++ show plot_start_date
    putStrLn $ " --end-date=" ++ show plot_end_date
    putStrLn $ " --year=" ++ show plot_year
    
    
    let plot_type = IncomeVsExpenses
    let plot_period = All
    loadData plot_type plot_period
    exitSuccess
