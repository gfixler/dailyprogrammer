import Data.List

everyNth :: Int -> [b] -> [b]
everyNth n = map snd . filter (\(a,b) -> a `mod` n == 0) . zip [1..]

everyOther = everyNth 2

padStr :: Int -> String -> String
padStr n s = take n (s ++ replicate n ' ')

rectStr :: String -> String
rectStr s = unlines . map (padStr longest) $ rows
    where rows = lines s
          longest = foldl max 0 (map length rows)

widenStr :: String -> String
widenStr = concatMap (\a -> [a,a])

heightenStr :: String -> String
heightenStr s = s ++ "\n" ++ s

zoomStr :: String -> String
zoomStr = unlines . map (heightenStr . widenStr) . lines

unzoomStr :: String -> String
unzoomStr = unlines . map everyOther . everyOther . lines

rot90Str :: String -> String
rot90Str = unlines . transpose . reverse . lines

unrot90Str :: String -> String
unrot90Str = unlines . reverse . transpose . lines

charInvert :: Char -> Char
charInvert c
    | c == 'x'  = ' '
    | c == ' '  = 'x'
    | otherwise = c

invertStr :: String -> String
invertStr = map charInvert

intermediate171 :: String -> String
intermediate171 = unzoomStr . invertStr . zoomStr . rot90Str . zoomStr . rectStr

upvote = "  xx\n xxxx\nxxxxxx\nxxxxxx\n  xx\n  xx\n  xx\n  xx"
target = "xxxxxxxx\nx      x\nx xxxx x\nx x  x x\nx x  x x\nx xxxx x\nx      x\nxxxxxxxx"
polka  = "x x x x \n x x x x\nx x x x \n x x x x\nx x x x \n x x x x\nx x x x \n x x x x"
moon   = "  xxxxx \n xxxxxxx\nxxxxxx  \nxxxxx   \nxxxxx   \nxxxxxx  \n xxxxxxx\n  xxxxx "
hello  = "x  x  xx\nx  x  xx\nx  x  xx\nxxxx  xx\nxxxx  xx\nx  x  xx\nx  x  xx\nx  x  xx"
