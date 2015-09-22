module Grid where

import qualified Data.Map as M (Map, keys, findWithDefault)

type Coord = (Int, Int)
type Grid a = M.Map Coord a

-- find the lower-left and upper-right Coords in a Grid
gridBounds :: Grid a -> (Coord, Coord)
gridBounds = foldr f e . M.keys
    where e = ((maxBound, maxBound), (minBound, minBound))
          f = \(x, y) ((xl, yl), (xh, yh)) -> ((min x xl, min y yl), (max x xh, max y yh))

-- get element - or given default - in Grid at key (x,y)
gridPtOr :: a -> Coord -> Grid a -> a
gridPtOr = M.findWithDefault


