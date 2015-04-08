module Main where

import Control.Applicative

readLines :: IO [String]
readLines = do
        l <- getLine
        if null l
            then return []
            else (l :) <$> readLines

readInts :: String -> [Int]
readInts = map read . words

readXYZ :: String -> (Int,Int,Int)
readXYZ s = let (x:y:z:[]) = readInts s in (x,y,z)

main = do
    s <- getLine
    getLine
    ls <- readLines
    let s' = readXYZ s
        ls' = map readXYZ ls
    print s'
    print ls'
