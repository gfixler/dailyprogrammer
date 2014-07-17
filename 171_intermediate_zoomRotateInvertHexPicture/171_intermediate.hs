import Data.List

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:ys) = x : everyOther ys

taller :: String -> String
taller = concatMap (\a -> [a,a])

wider :: String -> String
wider s = concat [s,"\n",s]

zoomIn :: String -> String
zoomIn s = concatMap ((++"\n") . taller . wider) $ lines s

zoomOutW :: String -> String
zoomOutW = everyOther

zoomOutH :: String -> String
zoomOutH = unlines . everyOther . lines

zoomOut :: String -> String
zoomOut = zoomOutW . zoomOutH

rotateCW :: String -> String
rotateCW = unlines . transpose . reverse . lines

rotateCCW :: String -> String
rotateCCW = unlines . reverse . transpose . lines

