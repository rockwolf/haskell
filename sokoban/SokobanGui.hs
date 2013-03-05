module SokobanGUI where

import Sokoban
import Prelude hiding (Either(..))
import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Control.Concurrent.MVar as MV
import Control.Monad (guard)

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

data State = State {sWorld :: World}

emptyState = State {sWorld = loadLevel level}

handleKeyBoard = do
    --[left, right, up, down] <- mapM (liftIO . keyvalFromName)
    --                                ["Left", "Right", "Up", "Down"]
    tryEvent $ do
        val <- keyvalName
        "Left" <- liftIO $ keyvalName val
        liftIO $ do
            print "Left"
     

main :: IO ()
main = do
    initGUI
   
    state <- MV.newMVar emptyState
    
    window <- windowNew
    window `on` keyPressEvent $ handleKeyBoard

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
