import System.Random (getStdRandom, randomR)

choose :: [a] -> IO a
choose xs = do
    i <- getStdRandom (randomR (0, length xs - 1))
    return $ xs !! i

data Corner    = TL | TR | BL | BR deriving (Eq, Show)
data Expansion = H | V deriving (Eq, Show)

data ExBox = ExBox (Int, Int) (Int, Int) Corner [(Int, Int)] deriving (Show)

exBox corner = ExBox (1,1) (0,0) corner [(0,0)]

expandPack :: Expansion -> ExBox -> ExBox
expandPack e (ExBox (w,h) (x,y) c ps) =
    case (e,c) of
        (H, TL) -> ExBox (w+1,h) (x-1,y+h-1) BL (ps ++ zip (repeat (x-1)) [y..(y+h-1)])
        (V, TL) -> ExBox (w,h+1) (x+w-1,y-1) TR (ps ++ zip [x..(x+w-1)] (repeat (y-1)))
        (H, TR) -> ExBox (w+1,h) (x+1,y+h-1) BR (ps ++ zip (repeat (x+1)) [y..(y+h-1)])
        (V, TR) -> ExBox (w,h+1) (x+w-1,y-1) TL (ps ++ zip (reverse [x..(x+w-1)]) (repeat (y-1)))
        (H, BL) -> ExBox (w+1,h) (x-1,y+h-1) TL (ps ++ zip (repeat (x-1)) (reverse [y..(y+h-1)]))
        (V, BL) -> ExBox (w,h+1) (x+w-1,y+1) BR (ps ++ zip [x..(x+w-1)] (repeat (y+1)))
        (H, BR) -> ExBox (w+1,h) (x+1,y+h-1) TR (ps ++ zip (repeat (x+1)) (reverse [y..(y+h-1)]))
        (V, BR) -> ExBox (w,h+1) (x+w-1,y+1) BL (ps ++ zip (reverse [x..(x+w-1)]) (repeat (y+1)))

