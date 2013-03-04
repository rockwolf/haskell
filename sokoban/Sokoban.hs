module Sokoban (modifyWorld
               ,isFinished
               ,loadLevel
               ,World
               ,Input(..)
               ,level
               ) where

import Prelude hiding (Either(..))
import Data.List (sort, delete)
import Control.Monad (forM_)

data Input = Up 
            | Down
            | Left
            | Right
            deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data World = World {
                    wWalls
                    ,wCrates
                    ,wStorage :: [Coord]
                    ,wWorker :: Coord
                    ,wMax :: Coord
                    ,wSteps :: Int
             }

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
    ["    #####"
    ,"    #   #"
    ,"    #o  #"
    ,"  ###  o##"
    ,"  #  o o #"
    ,"### # ## #   ######"
    ,"#   # ## #####  ..#"
    ,"# o  o          ..#"
    ,"##### ### #@##  ..#"
    ,"    #     #########"
    ,"    #######"
    ]

-- 0:49
loadLevel :: String -> World
loadLevel str =  foldl consume (emptyWorld{wMax = maxi}) elems
    where lns = lines str
          coords = [[(x,y) | x <- [0..]] | y <- [0..]]
          elems = concat $ zipWith zip coords lns
          maxX = maximum .map (fst . fst) $ elems
          maxY = maximum .map (snd . fst) $ elems
          maxi = (maxX, maxY)
          consume wld (c, elt) =
            case elt of
                '@' -> wld{wWorker  = c}
                'o' -> wld{wCrates  = c:wCrates wld}
                '#' -> wld{wWalls   = c:wWalls wld}
                '.' -> wld{wStorage = c:wStorage wld}
                ' ' -> wld
                otherwise -> error (show elt ++ " not recognized")

instance Show World where
    show w = unlines chars
        where (maxX, maxY)    = wMax w
              chars           = [[func (x,y) | x <- [0..maxX]]
                                             | y <- [0..maxY]]
              isWorker w c    = wWorker w == c
              func c
                | isCrate w c && isStorage w c   = '*'
                | isWorker w c && isStorage w c  = '+'
                | isWall w c                     = '#'
                | isWorker w c                   = '@'
                | isCrate w c                    = 'o'
                | isStorage w c                  = '.'
                | otherwise                      = ' '

-- continue with the refactor at 1:18 or something
modifyWorld :: World -> Input -> Maybe World
modifyWorld world input
    | isWall world newPos   = Nothing
    | isCrate world newPos  =
        if isCrate world newPos' || isWall world newPos' 
        then Nothing
        else return $ moveCrate world' newPos newPos'
    | otherwise             = return world'
    where
        moveCrate w old new =  w{wCrates = new:delete old (wCrates w)}
        oldPos = wWorker world
        newPos = add oldPos input
        newPos' = add newPos input 
        world' = world{wWorker = newPos, wSteps = wSteps world + 1}

-- all storage spaces covered by crates
isFinished :: World -> Bool
isFinished world = sort (wCrates world) == sort (wStorage world)

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (wWalls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (wCrates world)

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (wStorage world)

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
