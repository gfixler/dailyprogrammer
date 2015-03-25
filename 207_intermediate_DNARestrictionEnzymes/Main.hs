module Main (main, End, RE, toStrand) where

import Data.Char (toLower)
import System.Environment (getArgs)
import System.IO (readFile)

newtype Strand = Strand String deriving (Show)
data End = Sticky | Blunt
data RE = RE String End String String

toStrand :: String -> Strand
toStrand = Strand . filter (`elem` "gcta") . map toLower

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = toStrand contents
    print code

