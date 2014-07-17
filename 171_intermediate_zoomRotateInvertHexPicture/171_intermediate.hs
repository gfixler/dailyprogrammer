import Data.List

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:ys) = x : everyOther ys

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

