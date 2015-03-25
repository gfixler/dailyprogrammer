module Main (main, End, RE, toStrand) where

import Data.Char (toLower)
import System.Environment (getArgs)
import System.IO (readFile)

newtype Strand = Strand String deriving (Show)
data End = Sticky | Blunt deriving (Show)
data RE = RE String End Strand Strand deriving (Show)

toStrand :: String -> Strand
toStrand = Strand . filter (`elem` "acgt") . map toLower

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = toStrand contents
    print code

