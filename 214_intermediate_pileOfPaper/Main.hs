import Data.List.Split (chunksOf)
import Data.Map (Map, fromList, insertWith')
import System.IO (getContents)

type Sheet  = (Int, Int, Int, Int, Int) -- color, x, y, w, h
type Pile   = (Int, Int, Int, Int, [Sheet]) -- l, t, r, b, sheets

sheetCol :: Sheet -> Int
sheetCol (c,_,_,_,_) = c

onSheet :: Sheet -> Int -> Int -> Bool
onSheet (_,l,t,w,h) x y = x >= l && y >= t && x <= l+w-1 && y <= t+h-1

newPile :: Int -> Int -> Pile
newPile w h = (0,0,w,h,[])

pileOn :: Sheet -> Pile -> Pile
pileOn s@(c,x,y,w,h) (l,t,r,b,p) =
        (min x l, min y t, max (x+w) r, max (y+h) b, s:p)

pileSheets :: Pile -> [Sheet]
pileSheets (_,_,_,_,s) = s

pileWidth :: Pile -> Int
pileWidth (l,_,r,_,_) = r-l

colorAt :: Int -> Int -> Pile -> Int
colorAt x y (l,t,r,b,s) = f s
    where f [] = 0
          f (s:ss) = if onSheet s x y then sheetCol s else f ss

pileCols :: Pile -> [Int]
pileCols p@(l,t,r,b,s) = [colorAt x y p | y <- [t..b-1], x <- [l..r-1]]

strPile :: Pile -> String
strPile p = unlines . map concat . chunksOf (pileWidth p) $ cs
    where cs = map show $ pileCols p

colCounts :: Pile -> Map Int Int
colCounts p = foldr (\c -> insertWith' (+) c 1) (fromList []) (pileCols p)

readSpacedNums :: String -> [Int]
readSpacedNums = map read . words

main = do
    cs <- fmap lines getContents
    let [w,h] = readSpacedNums $ head cs
        ss    = map readSpacedNums $ tail cs
    let pile = foldl (\p [c,x,y,w,h] -> pileOn (c,x,y,w,h) p) (newPile w h) ss
    return $ colCounts pile

