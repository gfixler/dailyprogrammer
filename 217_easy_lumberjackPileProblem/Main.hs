import Data.List (sort, sortBy)
import Data.List.Split (chunksOf)
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
    let ps'    = toList $ (!! n) $ iterate pileup $ fromList (sort ps)
        xys    = [(x,y) | y <- [0..d], x <- [0..d]]
        coords = map snd $ sortBy (comparing fst) (zip ps xys)
        piles  = map fst $ sortBy (comparing snd) (zip ps' coords)
    mapM_ print (chunksOf d piles)
    return ()

