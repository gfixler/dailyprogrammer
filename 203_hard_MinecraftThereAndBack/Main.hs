module Main where

import Data.List (group)
import qualified Data.Map as M

type Cell = (Int, Int, Int)
data Block = Air | Dirt | Sand | Lava deriving (Eq, Show)
type World = (Cell, M.Map Cell Block)

genCells :: Cell -> [Cell]
genCells (l,w,h) = [(x,y,z) | x <- [0..l-1], y <- [0..w-1], z <- [0..h-1]]

newWorld :: Cell -> World
newWorld cell = (cell, M.fromList $ zip (genCells cell) (repeat Air))

worldSize :: World -> Cell
worldSize = fst

getBlock :: World -> Cell -> Maybe Block
getBlock cells cell = M.lookup cell (snd cells)

setBlock :: World -> Cell -> Block -> World
setBlock (s,w) c b = (s, M.adjust (const b) c w)

genCore :: Cell -> [Cell]
genCore (x,y,top) = [(x,y,z) | z <- [0..top]]

genCores :: World -> M.Map (Int,Int) [Cell]
genCores w = M.fromList [((x,y),genCore (x,y,top))
                        | x <- [0..x'], y <- [0..y']]
    where (x',y',top) = worldSize w

collapse :: [Block] -> [Block] -- [bottom..top] ordering
collapse xs = concat $ reorder $ group xs
    where reorder (a@(Air:_):s@(Sand:_):xs) = s : reorder (a:xs)
          reorder (a@(Air:_):l@(Lava:_):xs) = map (const Lava) a : l : reorder xs
          reorder (x:xs)                    = x : reorder xs
          reorder []                        = []

-- sampleCore :: World -> Int -> Int -> Maybe [Block]
-- sampleCore w x y = sequence $ map (getBlock w) $ core

