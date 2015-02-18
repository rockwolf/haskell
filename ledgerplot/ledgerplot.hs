{-# LANGUAGE OverloadedStrings #-}
--module Main (main) where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
import Data.Default.Class
import Control.Lens hiding (argument)
import Data.List.Split
import Data.List
import Control.Monad (when)
import System.Exit (exitSuccess)
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command,
    argument, longOption)
    
import DataConversion(convertListToListOfLists, splitLinesToElements, removeFirstFromGroupedList, removeLastFromGroupedList)
import FileIO(loadFileToStringList)
import DataPlot(plotBars)

-----------------------------------------------------------------------------
-- ||| Declaration of datatypes
-----------------------------------------------------------------------------
data PlotType = IncomeVsExpenses | Networth deriving (Show, Eq)
data PlotPeriod = All | AllMonthly | Year | YearMonthly | Period | PeriodMonthly deriving (Show, Eq)

-----------------------------------------------------------------------------
-- ||| Data loading for plot
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | loadDataFromFile
-- | Read the data from the file and put it in a list
-----------------------------------------------------------------------------
--loadDataFromFile :: FilePath -> PlotType -> IO [Double]
--loadDataFromFile file_name plot_type = do
--    file_data <- parseFileToStringList file_name plot_type
--    return $ convertListStringToDouble $ parseLinesToStringList file_data

-- | Load, transform and plot the data for the given PlotType and PlotPeriod
loadData :: PlotType -> PlotPeriod -> IO (PickFn ())
loadData plot_type plot_period = do
    file_data <- loadDataFromFile from_file plot_type
    let minimal_plot_data = map addDifferenceToList $ convertListToListOfLists file_data
    let plot_data = addMissingMonths minimal_plot_data
    renderableToFile def to_file $ plotBars plot_type plot_data title_main titles_series True
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
hasSummary :: PlotType -> Bool
hasSummary plot_type
    | plot_type == IncomeVsExpenses = True
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
addDifferenceToList :: Num t => [t] -> [t]
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

-- | Option parsing functions
--getPlotPeriodFromOptions :: PlotType -> Boolean -> PlotPeriod
--getPlotPeriodFromOptions plot_type plot_detail plot_year 
--  | (PlotType == IncomeVsExpenses) = if plot_detail then 
-- TODO: find a good way to determine the plotPeriod, based on the input options.  

-- ||| Main
main :: IO (PickFn ())
main = do
    -- TODO: implement command line parameter parsing with docopt
    opts <- optionsWithUsageFile "usage.txt"
    
    print opts
    putStrLn ""

    -- plot_type is gone after this... there is no state!
    when (opts `isPresent` (longOption "income-vs-expenses")) $ do
        let plot_type = IncomeVsExpenses
        putStrLn " --income-vs-expenses"
        when (opts `isPresent` (longOption "year")) $ do
            plot_year <- opts `getArg` (argument "year")
            putStrLn $ " --year=" ++ show plot_year
        when ((opts `isPresent` (longOption "start-date")) && (opts `isPresent` (longOption "end-date"))) $ do
            plot_start_date <- opts `getArg` (argument "start-date")
            putStrLn $ " --start-date=" ++ show plot_start_date
            plot_end_date <- opts `getArg` (argument "end-date")
            putStrLn $ " --end-date=" ++ show plot_end_date
        when (opts `isPresent` (longOption "detail")) $ do
            let plot_detail = True
            putStrLn " --detail"
    when (opts `isPresent` (longOption "networth")) $ do
        let plot_type = Networth
        putStrLn " --networth"
        when (opts `isPresent` (longOption "year")) $ do
            plot_year <- opts `getArg` (argument "year")
            putStrLn $ " --year=" ++ show plot_year
        when ((opts `isPresent` (longOption "start-date")) && (opts `isPresent` (longOption "end-date"))) $ do
            plot_start_date <- opts `getArg` (argument "start-date")
            putStrLn $ " --start-date=" ++ show plot_start_date
            plot_end_date <- opts `getArg` (argument "end-date")
            putStrLn $ " --end-date=" ++ show plot_end_date
        when (opts `isPresent` (longOption "detail")) $ do
            let plot_detail = True
            putStrLn " --detail"

    -- NOTE: you can put the below 3 lines in the when statements, if you end that when with a putStrLn.
    -- But this is too much repetition. A better way must be found.
    --let plot_type = IncomeVsExpenses
    --let plot_period = All
    --loadData plot_type plot_period
    exitSuccess
