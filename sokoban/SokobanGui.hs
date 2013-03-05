module SokobanGUI where

import Sokoban
import Prelude hiding (Either(..))
import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Control.Concurrent.MVar as MV
import Control.Monad (when, liftM)

data State = State {sWorld :: World
                    ,sInput :: Maybe Input
                    ,sOffset :: Double}

emptyState = State {sWorld = loadLevel level
                    ,sInput = Nothing
                    ,sOffset = 0}

handleKeyBoard window state = do
    --[left, right, up, down] <- mapM (liftIO . keyvalFromName)
    --                                ["Left", "Right", "Up", "Down"]
    tryEvent $ do
        "Left" <- (liftIO . keyvalName) =<< eventKeyVal
        checkInput Left
    tryEvent $ do
        "Right" <- (liftIO . keyvalName) =<< eventKeyVal
        checkInput Right
    tryEvent $ do
        "Up" <- (liftIO . keyvalName) =<< eventKeyVal
        checkInput Up
    tryEvent $ do
        "Down" <- (liftIO . keyvalName) =<< eventKeyVal
        checkInput Down
    where
        checkInput i = liftIO $ do
            MV.modifyMVar_ state (\s -> return s{sInput = return i})
            MV.modifyMVar_ state (\s -> return s{sOffset = sOffset s + 10})
            updateWorld state
            widgetQueueDraw window

updateWorld state = do
    s <- readMVar state
    let input = sInput s
        world = sWorld s
        world' = case modifyWorld world =<< input of
                    Nothing -> world
                    Just x  -> x
    MV.modifyMVar_ state (\s -> return s{sWorld = world'})
    when (isFinished world') $ print "Finished."
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

drawWindow window state = liftIO $ do
    cr <- widgetGetDrawWindow window
    offset <- liftM sOffset $ readMVar state
    renderWithDrawable cr $ do
        image <- liftIO $ C.imageSurfaceCreateFromPNG "images/Grass Block.png"
        C.setSourceSurface image offset offset
        C.paint
    return True
--continue at 0:41:21
     
main :: IO ()
main = do
    initGUI
   
    state <- MV.newMVar emptyState
    
    window <- windowNew
    window `on` sizeRequest     $ return (Requisition 1400 900)
    window `on` keyPressEvent   $ handleKeyBoard window state
    window `on` exposeEvent     $ drawWindow window state

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
