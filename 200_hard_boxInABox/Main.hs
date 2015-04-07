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

main = do
    s <- getLine
    getLine
    ls <- readLines
    let s' = readInts s
        ls' = map readInts ls
    print s'
    print ls'

