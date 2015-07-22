import qualified Data.Map as M (Map, fromList, filter)
import System.IO (getContents)

yLines :: String -> [(Int,String)]
yLines = zip [0..] . lines

xyLine :: (Int,String) -> [((Int,Int),Char)]
xyLine (y,s) = map (\(x,c) -> ((x,y),c)) (zip [0..] s)

xyLines :: String -> M.Map (Int,Int) Char
xyLines = M.fromList . concatMap xyLine . yLines

main = do
    c <- getContents
    return $ M.filter (/=' ') $ xyLines c

