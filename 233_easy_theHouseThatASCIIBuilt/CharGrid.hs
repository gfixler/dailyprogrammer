module CharGrid where

import qualified Data.Map as M (fromList)

import Grid

enum :: [a] -> [(Int, a)]
enum = zip [0..]

-- split string into lines enumerated with height values
toYLines :: String -> [(Int, String)]
toYLines = reverse . enum . reverse . lines

-- split row-enumerated line into (x,y) tagged Char list
yLineToXChars :: (Int, String) -> [(Coord, Char)]
yLineToXChars (y, s) = map (\(x,c) -> ((x,y),c)) (enum s)

-- split string into (x,y)-enumerated Chars
toCharGrid :: String -> Grid Char
toCharGrid = M.fromList . concatMap yLineToXChars . toYLines

-- assemble bbox-sized, "2D" string from Grid of Chars
renderCharGrid :: Grid Char -> String
renderCharGrid p = unlines [[gridPtOr ' ' (x, y) p | x <- [xl .. xh]] | y <- [yl .. yh]]
    where ((xl, yl), (xh, yh)) = gridBounds p


