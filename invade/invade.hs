module Main where

import System.IO
import System.Environment
import Data.List

boilerPlate :: [String]
boilerPlate = ["Case #" ++ show n ++ ": " | n <- [1..]]

standardOutput :: [String] -> [String]
standardOutput = zipWith (++) boilerPlate

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

main = do
    (p:_) <- getArgs
    pool <- readFile f

    let cases = tail $ lines file
        solutions = standardOutput $ map reverseWords cases
    
    putStrLn $ unlines $ solutions

    putStrLn $ (bmiTell 75.68 170)
