module Main where

import Data.List (group, transpose, intercalate)

-- turns multiline string into counts of vertical, grounded, asterisk columns
heights :: String -> [Int]
heights = map length . map (takeWhile (=='*')) . map reverse . transpose . lines

-- pairs up adjacent ints in a list; caps ends with 0s for pattern matching
heightPairs :: [Int] -> [(Int, Int)]
heightPairs xs = zip hs (tail hs)
    where hs = 0 : xs ++ [0]

-- repeats given char to given unit height, with some magic number foolery
vert :: Char -> Int -> String
vert c i = replicate (i*2-1) c

-- creates a building side vertical (left or right), to given unit height
side :: Int -> String
side i = '+' : vert '|' i ++ "+"

-- creates a building interior vertical, to given unit height
face :: Int -> String
face i = '-' : vert ' ' i ++ "-"

-- creates a building vertical where height changes, to given unit height
rise :: (Int, Int) -> String
rise (l, r) = lower ++ upper
    where lower = '-' : vert ' ' (min l r) ++ "+"
          upper = vert '|' (abs (l-r)) ++ "+"

-- choose/build a vertical strip of building, based on pair of unit heights
-- pair is used to detect building edges (0 values) for drawing side walls
vertical :: (Int, Int) -> String
vertical (l, r) | l == r    = face l
                | l == 0    = side r
                | r == 0    = side l
                | otherwise = rise (l, r)

-- creates a magic number of space-filling verticals to given unit height
horizontal :: Int -> [String]
horizontal n = replicate 3 (face n)

-- utility function to join lists together in a particular way
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- builds entire wall - verticals and space-fills - for list of heights
walls :: [Int] -> [String]
walls xs = concat (interleave [joins, walls])
    where joins = map (\x -> [vertical x]) (heightPairs xs)
          walls = map horizontal xs

-- builds up a given-unit-wide - times magic number - roof
roof :: Int -> [String]
roof w = last $ take w $ iterate ((["/"]++) . (++["\\"]) . (map (' ':))) ["A"]

-- builds and spaces out roofs for given list of heights
roofs :: [Int] -> [String]
roofs xs = [" "] ++ (intercalate [""] $ map (roof . (*2) . length) (group xs)) ++ [" "]

-- converts multiline stack of asterisks to building verticals (w/ roofs)
building :: String -> String
building s = upright $ zipWith (++) (walls hs) (roofs hs)
    where hs = heights s

-- utility function to right-space-pad string out to given length
pad :: Int -> String -> String
pad i s = s ++ replicate (i - length s) ' '

-- transpose-helper; right-space-pads list of strings to longest member
padBox :: [String] -> [String]
padBox xs = map (pad z) xs
    where z = maximum (map length xs)

-- pads/rotates string list counter-clockwise, merges to multiline string
upright :: [String] -> String
upright = unlines . reverse . transpose . padBox

-- example inputs for use with building function
input1 = "   *\n  ***\n******"
input2 = " *\n***\n***\n***\n***\n***\n***"
challenge1 = "    **\n*** **\n******"
challenge2 = "***                    ***\n***     **  ***  **    ***\n***   ***************  ***\n***   ***************  ***\n***   ***************  ***\n**************************\n**************************"

