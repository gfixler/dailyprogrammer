module Main (main, End, toStrand) where

import Data.Char (toLower)
import Data.List (findIndices)
import System.Environment (getArgs)
import System.IO (readFile)

newtype Strand = Strand String deriving (Show)
data End = Sticky | Blunt deriving (Show)
data RE = RE String End Strand Strand deriving (Show)

toStrand :: String -> Strand
toStrand = Strand . filter (`elem` "acgt") . map toLower

toRecSeq :: String -> Either String (Strand, Strand)
toRecSeq s = case findIndices (== '^') s of
                 []  -> Left ("No '^' in cut sequence " ++ s)
                 [n] -> Right (toStrand $ reverse $ take n s, toStrand $ drop (n+1) s)
                 _   -> Left ("More than 1 '^' in cut sequence " ++ s)

toRE :: String -> End -> String -> Either String RE
toRE n e s = case toRecSeq s of
                 Right (l, r) -> Right $ RE n e l r
                 Left x       -> Left x

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = toStrand contents
    print code

