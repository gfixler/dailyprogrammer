intSteps :: Int -> Int -> Int -> [Int]
intSteps s e n = map (round . remap) [0..n-1]
    where remap x = fromIntegral s + fromIntegral x * (fromIntegral e - fromIntegral s) / fromIntegral (n-1)

toPixmap :: [[(Int,Int,Int)]] -> String
toPixmap xss = header ++ image xss
    where header = "P3\n" ++ show width ++ " " ++ show (length xss) ++ " 255\n"
          width = minimum $ map length xss
          pixel (a,b,c) = unwords $ map show [a,b,c]
          line = unwords . map pixel
          image = unwords . map line

spacedInts :: String -> [Int]
spacedInts = map read . words

main = do
    [w,h] <- fmap spacedInts $ getLine
    [ar,ag,ab] <- fmap spacedInts $ getLine
    [br,bg,bb] <- fmap spacedInts $ getLine
    putStrLn "done"

