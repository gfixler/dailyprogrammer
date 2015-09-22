module Main where

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
toGrid :: String -> [(Coord, Char)]
toGrid = concatMap yLineToXChars . toYLines

-- assemble bbox-sized, "2D" string from Grid of Chars
renderCharPad :: Grid Char -> String
renderCharPad p = unlines [[gridPtOr ' ' (x, y) p | y <- [yl .. yh]] | x <- [xl .. xh]]
    where ((xl, yl), (xh, yh)) = gridBounds p

