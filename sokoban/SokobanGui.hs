module SokobanGUI where

import Sokoban
import Prelude hiding (Either(..))
import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk

{-
 gameLoop world = do
    print world
    input <- getInput
    -- NOTE: not calling isValid here,
    -- but just putting modifyWorld world input
    -- enables noclip mode! :) 
    --let world' = if isValid world input
    --             then modifyWorld world input
    --             else world
    let world' = case modifyWorld world input of
                    Just x -> x
                    Nothing -> world
    if isFinished world'
    then print "Awesome!"
    else gameLoop world'
-}

main :: IO ()
main = do
    initGUI
    
    window <- windowNew
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
