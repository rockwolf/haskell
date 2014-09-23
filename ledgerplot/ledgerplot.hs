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

chart :: String -> Bool -> Renderable ()
chart title borders = toRenderable layout
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
      $ plot_bars_values .~ addIndexes (loadData IncomeVsExpenses)
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

loadData :: PlotType -> [[Double]]
loadData pt
    | pt == IncomeVsExpenses = map addDifference $ [[20,45],[45,30],[30,20],[70,25],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45],[20,45]]
    | otherwise = [[0]]

loadAmounts :: FilePath -> IO [[Char]]
loadAmounts fileName = do
    lines <- readFile fileName
    let amounts = splitOn ";" lines
    --return $ map (filter (/= '"')) lines -- removes quotes
    return amounts

--addAmount :: IO [[Char]] -> IO [[Char]]
--addAmount amountList = do
--    fmap (sum 
--    return 

-- | Add difference to list
addDifferenceToList [] = []
addDifferenceToList [x] = [x]
addDifferenceToList (x:y:[]) = [x] ++ [y] ++ [x-y]
addDifferenceToList (x:y:xs) = [x] ++ [y] ++ [x-y] ++ (addDifferenceToList xs)

-- | Call addDifferenceToList for each list in the given list
addDifference (x:xs) = [x] ++ (addDifference xs)
addDifference [] = []

main :: IO (PickFn ())
main = do
    renderableToFile def "income_vs_expenses.png" $ chart "Income vs expenses" True
