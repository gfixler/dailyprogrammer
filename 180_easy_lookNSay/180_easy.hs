import Data.List

strhead :: String -> String
strhead = (:[]) . head

strlen :: String -> String
strlen = show . length

lookNSay :: String -> [(String,String)]
lookNSay s = zipWith (:) (map strlen g) (map strhead g)
    where g = group s

