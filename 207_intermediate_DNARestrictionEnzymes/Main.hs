module Main (main, End, toStrand) where

import Data.Char (toLower)
import System.Environment (getArgs)
import System.IO (readFile)

newtype Strand = Strand String deriving (Show)
data End = Sticky | Blunt deriving (Show)
data RE = RE String End Strand Strand deriving (Show)

toStrand :: String -> Strand
toStrand = Strand . filter (`elem` "acgt") . map toLower

toRE :: String -> End -> String -> RE
toRE n e s = RE n e l r
    where (l',r') = span (/= '^') s
          l = toStrand $ reverse l'
          r = toStrand $ tail r'

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = toStrand contents
    print code

