spacedInts :: String -> [Int]
spacedInts = map read . words

toPixmap :: Int -> Int -> [(Int,Int,Int)] -> String
toPixmap w h ps = "P3\n" ++ show w ++ " " ++ show h ++ "\n"

main = do
    [w,h] <- fmap spacedInts $ getLine
    [ar,ag,ab] <- fmap spacedInts $ getLine
    [br,bg,bb] <- fmap spacedInts $ getLine
    putStr (toPixmap w h [])

