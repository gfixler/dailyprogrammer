import qualified Data.Map as M (Map, fromList, keys, findWithDefault)

type Coord = (Int, Int)
type Grid a = M.Map Coord a

enum :: [a] -> [(Int, a)]
enum = zip [0..]

-- split string into lines enumerated with height values
toYLines :: String -> [(Int, String)]
toYLines = reverse . enum . reverse . lines

-- split row-enumerated line into (x,y) tagged Char list
yLineToXChars :: (Int, String) -> [(Coord, Char)]
yLineToXChars (y, s) = map (\(x,c) -> ((x,y),c)) (enum s)

-- split string into (x,y)-enumerated Chars
toGrid :: String -> [(Coord, Char)]
toGrid = concatMap yLineToXChars . toYLines

-- find the lower-left and upper-right Coords in a Grid
gridBounds :: Grid a -> (Coord, Coord)
gridBounds = foldr f e . M.keys
    where e = ((maxBound, maxBound), (minBound, minBound))
          f = \(x, y) ((xl, yl), (xh, yh)) -> ((min x xl, min y yl), (max x xh, max y yh))

-- get element - or given default - in Grid at key (x,y)
gridPtOr :: a -> Coord -> Grid a -> a
gridPtOr = M.findWithDefault

-- assemble bbox-sized, "2D" string from Grid of Chars
renderCharPad :: Grid Char -> String
renderCharPad p = unlines [[gridPtOr ' ' (x, y) p | y <- [yl .. yh]] | x <- [xl .. xh]]
    where ((xl, yl), (xh, yh)) = gridBounds p

