import Data.List
import Data.List.Split
import Data.Ord
import Data.Tuple
import System.Random (getStdRandom, randomR)

choose :: [a] -> IO a
choose xs = do
    i <- getStdRandom (randomR (0, length xs - 1))
    return $ xs !! i

data Corner    = TL | TR | BL | BR deriving (Eq, Show)
data Expansion = H | V deriving (Eq, Show)

data ExBox = ExBox (Int, Int, Int, Int) Corner [(Int, Int)] deriving (Show)

exBox corner = ExBox (0,0,0,0) corner [(0,0)]

expandPack :: Expansion -> ExBox -> ExBox
expandPack e (ExBox (l,t,r,b) c ps) =
    case (e,c) of
        (H, TL) -> ExBox (l-1,t,r,b) BL (ps ++ zip (repeat (l-1)) [t..b])
        (V, TL) -> ExBox (l,t-1,r,b) TR (ps ++ zip [l..r] (repeat (t-1)))
        (H, TR) -> ExBox (l,t,r+1,b) BR (ps ++ zip (repeat (r+1)) [t..b])
        (V, TR) -> ExBox (l,t-1,r,b) TL (ps ++ zip (reverse [l..r]) (repeat (t-1)))
        (H, BL) -> ExBox (l-1,t,r,b) TL (ps ++ zip (repeat (l-1)) (reverse [t..b]))
        (V, BL) -> ExBox (l,t,r,b+1) BR (ps ++ zip [l..r] (repeat (b+1)))
        (H, BR) -> ExBox (l,t,r+1,b) TR (ps ++ zip (repeat (r+1)) (reverse [t..b]))
        (V, BR) -> ExBox (l,t,r,b+1) BL (ps ++ zip (reverse [l..r]) (repeat (b+1)))

showExBox :: ExBox -> String -> String
showExBox (ExBox (l,_,r,_) _ xs) s =
    unlines . chunksOf (r-l+1) . map snd . sortBy (comparing (swap . fst)) $ zip xs s

