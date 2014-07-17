import Data.List

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:ys) = x : everyOther ys

widenStr :: String -> String
widenStr s = concatMap (\a -> [a,a]) s

heightenStr :: String -> String
heightenStr s = s ++ "\n" ++ s

zoomIn :: String -> String
zoomIn = unlines . map (heightenStr . widenStr) . lines

zoomOut :: String -> String
zoomOut = everyOther . unlines . map everyOther . lines

rot90Str :: String -> String
rot90Str = unlines . transpose . reverse . lines

unrot90Str :: String -> String
unrot90Str = unlines . reverse . transpose . lines

