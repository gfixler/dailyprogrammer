module Main (main) where

import Parse (parseTransition, parse)
import Turing ()
import System.Environment (getArgs)

main = do
    [f] <- getArgs
    d   <- parse $ readFile f
    print d

