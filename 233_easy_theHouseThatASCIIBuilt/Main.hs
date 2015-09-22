import qualified Data.Map as M (Map, fromList, keys, findWithDefault)

type Coord = (Int, Int)
type Grid a = M.Map Coord a

enum :: [a] -> [(Int, a)]
enum = zip [0..]

toYLines :: String -> [(Int, String)]
toYLines = reverse . enum . reverse . lines

yLineToXChars :: (Int, String) -> [(Coord, Char)]
yLineToXChars (y, s) = map (\(x,c) -> ((x,y),c)) (enum s)

toGrid :: String -> [(Coord, Char)]
toGrid = concatMap yLineToXChars . toYLines

gridBounds :: Grid a -> (Coord, Coord)
gridBounds = foldr f e . M.keys
    where e = ((maxBound, maxBound), (minBound, minBound))
          f = \(x, y) ((xl, yl), (xh, yh)) -> ((min x xl, min y yl), (max x xh, max y yh))

gridPtOr :: a -> Coord -> Grid a -> a
gridPtOr = M.findWithDefault

renderCharPad :: Grid Char -> String
renderCharPad p = unlines [[gridPtOr ' ' (x, y) p | y <- [yl .. yh]] | x <- [xl .. xh]]
    where ((xl, yl), (xh, yh)) = gridBounds p

