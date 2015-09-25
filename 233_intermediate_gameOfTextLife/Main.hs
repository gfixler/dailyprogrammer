module Main where

import qualified Data.Map as M (Map, fromList, keys, lookup)

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

-- finds lower-left and upper-right coords as a pair of pairs of Ints
bbox :: CharGrid -> (Coord, Coord)
bbox m = ((xl, yl), (xh, yh))
    where ks = M.keys m
          xs = map fst ks
          ys = map snd ks
          xl = minimum xs
          yl = minimum ys
          xh = maximum xs
          yh = maximum ys

alive :: CharGrid -> Coord -> Bool
alive g c = case (M.lookup c g) of Nothing  -> False
                                   Just ' ' -> False
                                   _        -> True

