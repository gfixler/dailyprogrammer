module Main (main, End, filterForDNA, makeCutter, toRECutter) where

import Data.Char (toLower)
import Data.List (findIndices)
import System.Environment (getArgs)
import System.IO (readFile)

newtype Strand = Strand String deriving (Show)
data End = Sticky | Blunt deriving (Show)
data RE = RE String End Strand Strand deriving (Show)

makeCutter :: String -> Either String (Strand, Strand)
makeCutter s = case findIndices (== '^') s of
                 []  -> Left ("No cut marker ('^') in recognition sequence " ++ s)
                 [n] -> Right (filterForDNA $ reverse $ take n s, filterForDNA $ drop (n+1) s)
                 _   -> Left ("More than 1 cut marker ('^') in recognition sequence " ++ s)

toRECutter :: String -> End -> String -> Either String RE
toRECutter n e s = case makeCutter s of
                 Right (l, r) -> Right $ RE n e l r
                 Left x       -> Left x

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = filterForDNA contents
    print code

filterForDNA :: String -> Strand
filterForDNA = flip Strand [] . filter (`elem` "acgt") . map toLower

