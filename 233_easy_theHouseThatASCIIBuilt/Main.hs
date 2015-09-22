module Main where

import Data.List (transpose)

sumHeights :: Num a => [a] -> [a] -> [a]
sumHeights xs [] = xs
sumHeights [] ys = ys
sumHeights (x:xs) (y:ys) = x + y : sumHeights xs ys

height :: Char -> Int
height '*' = 1
height _   = 0

heights :: String -> [Int]
heights = foldr1 sumHeights . map (map height) . lines

input1 = "   *\n  ***\n******"
input2 = " *\n***\n***\n***\n***\n***\n***"
challenge1 = "    **\n*** **\n******"
challenge2 = "***                    ***\n***     **  ***  **    ***\n***   ***************  ***\n***   ***************  ***\n***   ***************  ***\n**************************\n**************************"

