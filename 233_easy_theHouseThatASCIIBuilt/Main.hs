module Main where

import System.IO (getContents)

import CharGrid

main = getContents >>= putStrLn . renderCharGrid . toCharGrid

