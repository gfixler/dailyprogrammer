module Main where

import Data.List (transpose)

type HeightPair = (Int, Int)

sumHeights :: Num a => [a] -> [a] -> [a]
sumHeights xs [] = xs
sumHeights [] ys = ys
sumHeights (x:xs) (y:ys) = x + y : sumHeights xs ys

heights :: String -> [Int]
heights = foldr1 sumHeights . map (map height) . lines
    where height x = if x == ' ' then 0 else 1

heightPairs :: [Int] -> [HeightPair]
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

vertical :: HeightPair -> String
vertical (l, r) | l == r    = face l
             | l == 0    = side r
             | r == 0    = side l
             | otherwise = rise (l, r)

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

