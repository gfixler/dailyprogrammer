module Main where

import Data.List (transpose)

heights :: String -> [Int]
heights = map length . map (takeWhile (=='*')) . map reverse . transpose . lines

heightPairs :: [Int] -> [(Int, Int)]
heightPairs xs = zip hs (tail hs)
    where hs = 0 : xs ++ [0]

vert :: Char -> Int -> String
vert c i = replicate (i*2-1) c

side :: Int -> String
side i = '+' : vert '|' i ++ "+"

face :: Int -> String
face i = '-' : vert ' ' i ++ "-"

rise :: (Int, Int) -> String
rise (l, r) = lower ++ upper
    where lower = '-' : vert ' ' (min l r) ++ "+"
          upper = vert '|' (abs (l-r)) ++ "+"

vertical :: (Int, Int) -> String
vertical (l, r) | l == r    = face l
             | l == 0    = side r
             | r == 0    = side l
             | otherwise = rise (l, r)

horizontal :: Int -> [String]
horizontal n = replicate 3 (face n)

interleave :: [[a]] -> [a]
interleave = concat . transpose

roof :: Int -> [String]
roof w = last $ take w $ iterate ((["/"]++) . (++["\\"]) . (map (' ':))) ["A"]

pad :: Int -> String -> String
pad i s = s ++ replicate (i - length s) ' '

padBox :: [String] -> [String]
padBox xs = map (pad z) xs
    where z = maximum (map length xs)

upright :: [String] -> String
upright = unlines . reverse . transpose . padBox

input1 = "   *\n  ***\n******"
input2 = " *\n***\n***\n***\n***\n***\n***"
challenge1 = "    **\n*** **\n******"
challenge2 = "***                    ***\n***     **  ***  **    ***\n***   ***************  ***\n***   ***************  ***\n***   ***************  ***\n**************************\n**************************"

