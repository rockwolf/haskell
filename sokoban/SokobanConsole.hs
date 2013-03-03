module SokobanConsole where

import Sokoban
import Prelude hiding (Either(..))
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'k' -> return Up
        'h' -> return Left
        'j' -> return Down
        'l' -> return Right
        'w' -> return Up
        'a' -> return Left
        'r' -> return Down
        's' -> return Right
        otherwise -> getInput

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    gameLoop $ loadLevel level

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
    then print world' >> print "Awesome!"
    else gameLoop world'
