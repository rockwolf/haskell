{- |
Module : GInvade.hs
Description : Investment and trading calculations.
Copyright : (c) <Andy Nagels>
License : See LICENSE file for license details.

Maintainer : <no@e-mail.given>
Stability : unstable
Portability : portable

Gui interface to invade.
-}

--module GInvade where

import Invade
import System.IO (stdin, stderr, stdout, hSetEcho, hPutStrLn, hSetBuffering, BufferMode(..))
import System.Exit
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

{- GUI interfacing -}

main :: IO ()
main = do
    -- Create the gui
    initGUI
        Just xml <- xmlNew "GInvade.glade"
        window   <- xmlGetWidget xml castToWindow "window1"
        onDestroy window mainQuit

        -- signals
        -- TODO: put signals here
        -- TODO: add close button to the gui
        closeButton <- xmlGetWidget xml castToButton "button2"
        onClicked closeButton $ do
            widgetDestroy window
            putStrLn "Close Button Clicked"
       
        --opts <- foldl (>>=) (return defaultInput) actions

        widgetShowAll window
        mainGUI -- should always be last
