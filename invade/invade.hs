module Main where

import System.IO
import System.Environment
import Data.List

boilerPlate :: [String]
boilerPlate = ["Case #" ++ show n ++ ": " | n <- [1..]]

standardOutput :: [String] -> [String]
standardOutput = zipWith (++) boilerPlate

main = do
    (p:_) <- getArgs
    pool <- readFile f

    let cases = tail $ lines file
        solutions = standardOutput $ map reverseWords cases
    
    putStrLn $ unlines $ solutions
