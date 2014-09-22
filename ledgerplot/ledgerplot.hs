
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.SRGB
import Diagrams.Attributes
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
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
    | pt == IncomeVsExpenses = [[20,45,10],[45,30,10],[30,20,10],[70,25,10],[20,45,10],[20,45,10],[20,45,10],[20,45,10],[20,45,10],[20,45,10],[20,45,10],[20,45,10]]
    | otherwise = [[0]]


main :: IO (PickFn ())
main = do
    renderableToFile def "income_vs_expenses.png" $ chart "Income vs expenses" True
