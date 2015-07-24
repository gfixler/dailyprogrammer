import qualified Data.Map as M (Map, fromList, filter)
import System.IO (getContents)

numRows :: String -> [(Int,String)]
numRows = zip [0..] . lines

numCols :: (Int,String) -> [((Int,Int),Char)]
numCols (y,s) = map (\(x,c) -> ((x,y),c)) (zip [0..] s)

numRowsCols :: String -> M.Map (Int,Int) Char
numRowsCols = M.fromList . concatMap numCols . numRows

main = do
    c <- getContents
    return $ M.filter (/=' ') $ numRowsCols c

