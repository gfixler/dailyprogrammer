intSteps :: Int -> Int -> Int -> [Int]
intSteps s e n = map (round . remap) [0..n-1]
    where fi = fromIntegral -- longest function name ever
          remap x = fi s + fi x * (fi e - fi s) / fi (n-1)

toPixmap :: [[(Int,Int,Int)]] -> String
toPixmap xss = header ++ image xss
    where header = "P3\n" ++ show width ++ " " ++ show (length xss) ++ " 255\n"
          width = minimum $ map length xss
          pixel (a,b,c) = unwords $ map show [a,b,c]
          line = unwords . map pixel
          image = unwords . map line

hgrad3 :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> [(Int,Int,Int)]
hgrad3 (a,b,c) (d,e,f) n = zip3 (intSteps a d n)
                                (intSteps b e n)
                                (intSteps c f n)

spacedInts :: String -> [Int]
spacedInts = map read . words

main = do
    [w,h] <- fmap spacedInts $ getLine
    [ar,ag,ab] <- fmap spacedInts $ getLine
    [br,bg,bb] <- fmap spacedInts $ getLine
    putStrLn "done"

