import Data.List

everyNth :: Int -> [b] -> [b]
everyNth n = map snd . filter (\(a,b) -> a `mod` n == 0) . zip [1..]

everyOther = everyNth 2

widenStr :: String -> String
widenStr = concatMap (\a -> [a,a])

heightenStr :: String -> String
heightenStr s = s ++ "\n" ++ s

zoomStr :: String -> String
zoomStr = unlines . map (heightenStr . widenStr) . lines

unzoomStr :: String -> String
unzoomStr = everyOther . unlines . map everyOther . lines

rot90Str :: String -> String
rot90Str = unlines . transpose . reverse . lines

unrot90Str :: String -> String
unrot90Str = unlines . reverse . transpose . lines

