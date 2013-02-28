module Sokoban where

import Test.HUnit
import Test.QuickCheck
import Prelude hiding (Either(..))
import Data.List (sort)
import Control.Monad (forM_)

type Coord = (Int, Int)

data World = World {
                    wWalls
                    ,wCrates
                    ,wStorage :: [Coord]
                    ,wWorker :: Coord
                    ,wMax :: Coord
                    ,wSteps :: Int
             } deriving (Show)

emptyWorld = World {
                    wWalls = []
                    ,wCrates = []
                    ,wStorage = []
                    ,wWorker = (0,0)
                    ,wMax = (0,0)
                    ,wSteps = 0
            }

add :: Coord -> Input -> Coord
add (x,y) input =
    case input of
    Up      -> (x, y-1)
    Down    -> (x, y+1)
    Left    -> (x-1, y)
    Right   -> (x+1, y)

level = unlines
    ["#####" -- [((0,0), '#'), ((1,0), '#'), ...]
    ,"#.o@#"
    ,"#####"
    ]

-- 0:49
loadLevel :: String -> World
loadLevel str =  foldl consume (emptyWorld{wMax = maxi}) elems
    where lns = lines str
          coords = [[(x,y) | x <- [0..]] | y <- [0..]]
          elems = concat $ zipWith zip coords lns
          maxi = fst . last $ elems
          consume wld (c, elt) =
            case elt of
                '@' -> wld{wWorker  = c}
                'o' -> wld{wCrates  = c:wCrates wld}
                '#' -> wld{wWalls   = c:wWalls wld}
                '.' -> wld{wStorage = c:wStorage wld}
                ' ' -> wld
                otherwise -> error (show elt ++ " not recognized")

-- at 01:06
displayWorld :: World -> IO ()
displayWorld w = forM_ coords $ \c -> do
    putStr $ case () of () | isCrate w c && isStorage w c   -> "*"
                           | isWorker w c && isStorage w c  -> "+"
                           | isWall w c                     -> "#"
                           | isWorker w c                   -> "@"
                           | isCrate w c                    -> "o"
                           | isStorage w c                  -> "."
                           | otherwise                      -> " "
    where (maxX, maxY) = wMax w
          coords       = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
          isWorker w c = wWorker w == c

modifyWorld :: World -> Input -> World
modifyWorld world input = world

data Input = Up 
            | Down
            | Left
            | Right
            deriving (Show, Eq, Ord)

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'w' -> return Up
        'a' -> return Left
        'r' -> return Down
        's' -> return Right
        otherwise -> getInput

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (wWalls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (wCrates world)

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (wStorage world)

{-
    case newPos of
        empty       -> True
        isWall      -> False
        isCrate     -> isEmpty newPos' || isStorage newPos'
    where
        newPos = oldPos + input
        newPos' = newPos + input 
 -}
isValid :: World -> Input -> Bool
isValid world input =
    case () of ()   | isWall world newPos      -> False
                    | isCrate world newPos     -> not (isCrate world newPos') && not (isWall world newPos')
                    | otherwise                -> True
    where
        oldPos = wWorker world
        newPos = add oldPos input
        newPos' = add newPos input 

-- all storage spaces covered by crates
isFinished :: World -> Bool
isFinished world = sort (wCrates world) == sort (wStorage world)

main :: IO ()
main = gameLoop $ loadLevel level

gameLoop world = do
    displayWorld world
    input <- getInput
    let world' = if isValid world input
                then modifyWorld world input
                else world
    if isFinished world'
    then displayWorld world' >> print "Awesome!"
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
