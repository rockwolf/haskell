module Sokoban where

import Test.HUnit
import Test.QuickCheck

type Coord = (Int, Int)

data World = World {
                    wWalls
                    ,wCrates
                    ,wStorage :: [Coord]
                    ,wWorker :: Coord
                    ,wSteps :: Int
             }

data Input

loadLevel :: IO World
loadLevel = undefined

getInput :: IO Input
getInput = undefined

isValid :: World -> Input -> Bool
isValid = undefined

modifyWorld :: World -> Input -> World
modifyWorld = undefined

-- 1 - Sokoban - Haskell Live Coding 0:13:27

displayWorld :: World -> IO ()
displayWorld = undefined

isFinished :: World -> Bool
isFinished = undefined

main :: IO ()
main = gameLoop =<< loadLevel

gameLoop world = do
    input <- getInput
    let world' = if isValid world input
                then modifyWorld world input
                else world
    displayWorld world'
    if isFinished world'
    then print "Awesome!"
    else gameLoop world'



{-
 
load level
while not finished
{
    get input
    gameStep input
    {
        if isValid input
            modifyWorld
    }
    display level
}
------
walls --static
crates
storage spaces
warehouse man
[number of steps]
-}

-- TESTS

testsSoko = TestList $ map TestCase
    [assertEqual "" 1
                    1
    ]

prop_empty c1 = (c1::Int) == c1

runTests = do
    runTestTT testsSoko
    quickCheck prop_empty
