module Main where

import Data.List (transpose)

sumHeights :: Num a => [a] -> [a] -> [a]
sumHeights xs [] = xs
sumHeights [] ys = ys
sumHeights (x:xs) (y:ys) = x + y : sumHeights xs ys

height :: Char -> Int
height ' ' = 0
height _   = 1

heights :: String -> [Int]
heights = foldr1 sumHeights . map (map height) . lines

heightPairs :: [Int] -> [(Int, Int)]
heightPairs xs = zip hs (tail hs)
    where hs = 0 : xs ++ [0]

vert :: Char -> Int -> String
vert c i = replicate (i*2-1) c

side :: Int -> String
side i = '+' : vert '|' i ++ "+"

face :: Int -> String
face i = '-' : vert ' ' i ++ "-"

joint :: (Int, Int) -> String
joint (0, r) = side r
joint (l, 0) = side l
joint (l, r) | l == r = face l
             | otherwise = face (min l r) ++ vert '|' (abs (l-r)) ++ "+"

pad :: Int -> String -> String
pad i s = s ++ replicate (i - length s) ' '

padBox :: [String] -> [String]
padBox xs = map (pad z) xs
    where z = maximum (map length xs)

input1 = "   *\n  ***\n******"
input2 = " *\n***\n***\n***\n***\n***\n***"
challenge1 = "    **\n*** **\n******"
challenge2 = "***                    ***\n***     **  ***  **    ***\n***   ***************  ***\n***   ***************  ***\n***   ***************  ***\n**************************\n**************************"

