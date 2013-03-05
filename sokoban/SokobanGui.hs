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

data State = State {sWorld :: World
                    ,sInput :: Maybe Input}

emptyState = State {sWorld = loadLevel level
                    ,sInput = Nothing}

handleKeyBoard state = do
    --[left, right, up, down] <- mapM (liftIO . keyvalFromName)
    --                                ["Left", "Right", "Up", "Down"]
    checkInput "Left"   Left
    checkInput "Right"  Right
    checkInput "Up"     Up
    checkInput "Down"   Down
    where
        checkInput key i = tryEvent $ do
            "Left" <- (liftIO . keyvalName) =<< eventKeyVal
            liftIO $ do
                MV.modifyMVar_ state (\s -> return s{sInput = return i})

drawWindow window state = liftIO $ do
    cr <- widgetGetDrawWindow window
    renderWithDrawable cr $ do
        image <- liftIO $ C.imageSurfaceCreateFromPNG "images/Grass Block.png"
        C.setSourceSurface image 0 0
        C.paint
        return True
     
main :: IO ()
main = do
    initGUI
   
    state <- MV.newMVar emptyState
    
    window <- windowNew
    window `on` sizeRequest     $ return (Requisition 1400 900)
    window `on` keyPressEvent   $ handleKeyBoard state
    window  `on` exposeEvent    $ drawWindow window state

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
