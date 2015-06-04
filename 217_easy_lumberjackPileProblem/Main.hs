import Data.List (sortBy)
import Data.List.Zipper (fromList)
import Data.Ord (comparing)
import System.IO (getContents)

readLinesOfInts :: String -> [Int]
readLinesOfInts = concat . map (map read . words) . lines

-- example usage: cat input | runhaskell Main.hs
main :: IO ()
main = do
    (d:n:ps) <- fmap readLinesOfInts getContents
    let xys = [(x,y) | y <- [0..d], x <- [0..d]]
        ps' = fromList $ sortBy (comparing fst) (zip ps xys)
    print ps'
    return ()

