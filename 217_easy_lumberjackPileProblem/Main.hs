import System.IO (getContents)

readLinesOfInts :: String -> [Int]
readLinesOfInts = concat . map (map read . words) . lines

-- example usage: cat input | runhaskell Main.hs
main :: IO ()
main = do
    (d:n:ps) <- fmap readLinesOfInts getContents
    print d
    print n
    print ps
    return ()

