module Main (main) where

import Parse (parseTransition, parse)
import Turing (trans)
import Data.Either (lefts)
import System.Environment (getArgs)

main = do
    [file] <- getArgs
    machine <- parse $ readFile file
    mapM_ (\x -> if null x then return () else error ("bad rule: \"" ++ x ++ "\"")) (lefts $ trans machine)

