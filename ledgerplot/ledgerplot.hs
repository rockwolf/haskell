module Ledgerplot (ledgerplot) where

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

-- | Plotting related functions
chart :: PlotType -> String -> Bool -> Renderable ()
chart plot_type title borders = toRenderable layout
 where
  layout = 
        layout_title .~ "Plot " ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars plotData ]
      $ def :: Layout PlotIndex Double

  plotData = plot_bars_titles .~ ["Expenses","Income","P/L"]
      $ plot_bars_values .~ addIndexes (loadData plot_type)
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

-- | Data loading functions
loadData :: PlotType -> [[Double]]
loadData plot_type
--    | pt == IncomeVsExpenses = map addDifferenceToList [[20,45],[45,30],[30,20],[70,25],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45]]
    | plot_type == IncomeVsExpenses = map addDifferenceToList $ loadDataFromFile plot_type "testdata.dat"
    | otherwise = [[0]]

loadDataFromFile :: PlotType -> FilePath -> [[Double]]
loadDataFromFile plot_type file_name = do
    file_data <- parseFileToStringList file_name
    let chart_data = convertListStringToDouble file_data 
    return chart_data

-- | Data parsing functions
parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" c

parseFileToStringList :: FilePath -> IO [String]
parseFileToStringList filename = do
  my_data <- readFile filename
  return (lines my_data)

-- | Conversion functions
convertListStringToDouble = map convertToDouble

-- TODO: use reads?
convertToDouble aString = read aString :: Double

-- | Add difference to list
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

-- | Turn list into list of lists (2 pairs)
convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

-- | Main function
ledgerplot :: IO (PickFn ())
ledgerplot = do
    renderableToFile def "income_vs_expenses.png" $ chart IncomeVsExpenses "Income vs expenses" True
