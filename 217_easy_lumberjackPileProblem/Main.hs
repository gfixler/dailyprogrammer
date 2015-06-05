import Data.List (sortBy)
import Data.List.Zipper ( Zipper(Zip), fromList, toList
                        , cursor, replace, endp
                        , start, left, right)
import Data.Ord (comparing)
import System.IO (getContents)

readLinesOfInts :: String -> [Int]
readLinesOfInts = concat . map (map read . words) . lines

zsucc :: Enum a => Zipper a -> Zipper a
zsucc z = replace (succ $ cursor z) z

pileup :: (Enum a, Ord a) => Zipper a -> Zipper a
pileup z | (endp . right) z              = start (zsucc z)
         | cursor z < (cursor . right) z = start (zsucc z)
         | otherwise                     = (right . zsucc) z

-- example usage: cat input | runhaskell Main.hs
main :: IO ()
main = do
    (d:n:ps) <- fmap readLinesOfInts getContents
    let xys = [(x,y) | y <- [0..d], x <- [0..d]]
        ps' = fromList $ sortBy (comparing fst) (zip ps xys)
    print ps'
    return ()

