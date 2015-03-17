module Main where

import System.Environment

main = fmap (unlines . format . relate . parse . reverse) getArgs >>= putStr

format :: [Int] -> [String]
format = zipWith f [0..]
    where f a b = "Term " ++ show a ++ ": " ++ show b

relate :: (Int, Int, (Int -> Int)) -> [Int]
relate (s,n,t) = take n $ iterate t s

parse :: [String] -> (Int, Int, (Int -> Int))
parse (n:s:ts) = (read s, read n, c)
    where c = foldr (.) id (map toTerm ts)

toTerm :: String -> (Int -> Int)
toTerm (o:t) = flip op (read t)
    where op = case o of
                   '+' -> (+)
                   '-' -> (-)
                   '*' -> (*)
                   '/' -> div

