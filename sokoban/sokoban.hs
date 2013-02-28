module Sokoban where

import Test.HUnit
import Test.QuickCheck
import Prelude hiding (Either(..))
import Data.List (sort)
import Control.Monad (forM_)
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

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

displayWorld :: World -> IO ()
displayWorld w = putStrLn . unlines . map (map func) $ coords
    where (maxX, maxY)  = wMax w
          coords          = [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
          isWorker w c    = wWorker w == c
          func c          =
            case () of () | isCrate w c && isStorage w c   -> '*'
                          | isWorker w c && isStorage w c  -> '+'
                          | isWall w c                     -> '#'
                          | isWorker w c                   -> '@'
                          | isCrate w c                    -> 'o'
                          | isStorage w c                  -> '.'
                          | otherwise                      -> ' '

moveCrate :: (Int, Int) -> (Int, Int) -> World
moveCrate = undefined

-- continue with the refactor at 1:18 or something
modifyWorld :: World -> Input -> World
modifyWorld world input =
    case () of ()   | isWall world newPos      -> world
                    | isCrate world newPos ->
                        if not (isCrate world newPos') && not (isWall world newPos')
                        then moveCrate newPos newPos'
                        else world
                    | otherwise                -> world
    where
        oldPos = wWorker world
        newPos = add oldPos input
        newPos' = add newPos input 
        world' = world{wWorker = newPos, wSteps = wSteps world + 1}

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
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    gameLoop $ loadLevel level

gameLoop world = do
    displayWorld world
    input <- getInput
    let world' = modifyWorld world input
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
