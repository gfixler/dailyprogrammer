module Main where

import qualified Data.Map as M (Map, fromList)

type CharGrid = M.Map (Int, Int) Char
type Coord = (Int, Int)

-- maps XY positions in [potentially multiline] String to their Chars
stringrid :: String -> CharGrid
stringrid = M.fromList . g (0, 0)
    where g :: (Int, Int) -> String -> [((Int, Int), Char)]
          g _ []             = []
          g (x, y) ('\n':cs) = g (0, succ y) cs
          g (x, y) (' ':cs)  = g (succ x, y) cs
          g (x, y) (c:cs)    = ((x, y), c) : g (succ x, y) cs

