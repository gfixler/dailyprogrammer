module Main where

import qualified Data.Map as M
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

type Cell = (Int, Int, Int)
data Block = Air | Dirt | Sand | Lava deriving (Show)
type World = M.Map Cell Block

genCells :: Cell -> [Cell]
genCells (l,w,h) = [(x,y,z) | x <- [0..l-1], y <- [0..w-1], z <- [0..h-1]]

newWorld :: Cell -> World
newWorld cell = M.fromList $ zip (genCells cell) (repeat Air)

worldSize :: World -> Cell
worldSize w = last $ M.keys w

getBlock :: World -> Cell -> Maybe Block
getBlock cells cell = M.lookup cell cells

setBlock :: World -> Cell -> Block -> World
setBlock w c b = M.adjust (const b) c w

