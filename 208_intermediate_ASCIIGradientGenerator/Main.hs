import System.IO

clamp :: Ord a => a -> a -> a -> a
clamp l h v = min h (max l v)

clampSelect :: RealFrac a => [b] -> a -> a -> a -> b
clampSelect xs l h v = xs !! x
    where s = ((clamp l h v) - l) / (h - l)
          x = round $ s * fromIntegral (length xs - 1)

hypot2D :: Floating a => (a, a) -> (a, a) -> a
hypot2D (x1,y1) (x2,y2) = sqrt (x^2 + y^2)
    where x = x2 - x1
          y = y2 - y1

radGradVal :: (Floating a, RealFrac a) => [b] -> (a, a) -> a -> a -> (a, a) -> b
radGradVal cs (cx,cy) ri ro (x,y) = clampSelect cs ri ro d
    where d = hypot2D (cx,cy) (x,y)

renderRadial :: (Enum a, Floating a, RealFrac a) => [b] -> a -> a -> (a, a) -> a -> a -> [[b]]
renderRadial cs w h cp ri ro = [[radGradVal cs cp ri ro (x,y)
                               | x <- [0..w]] | y <- [0..h]]

data World = World { width :: Int
                   , height :: Int
                   , x     :: Int
                   , y     :: Int
                   , ri     :: Int
                   , ro     :: Int
                   , s      :: [Char]
                   }

instance Show World where
    show w = "wh(" ++ show (width w) ++ "," ++ show (height w) ++ ")"
           ++ " xy(" ++ show (x w) ++ "," ++ show (y w) ++ ")"
           ++ " i" ++ show (ri w)
           ++ " o" ++ show (ro w)
           ++ " " ++ show (s w)

worldDefault :: World
worldDefault = World 44 22 0 0 0 4 " .,;+%#@"

data Mode = Window | RadCen | RadIn | RadOut deriving (Show)

main = do
    let w = worldDefault
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdout False
    loop Window w

loop mode a@(World w h x y i o s) = do
    print a
    print mode
    k <- getChar
    case k of
        'q' -> return ()
        'w' -> loop Window $ World w h x y i o s
        'c' -> loop RadCen $ World w h x y i o s
        'i' -> loop RadIn $ World w h x y i o s
        'o' -> loop RadOut $ World w h x y i o s
        'h' -> loop mode $ case mode of
                          Window -> World (max 1 (pred w)) h x y i o s
                          RadCen -> World w h (pred x) y i o s
                          RadIn  -> World w h x y (max 0 (pred i)) o s
                          RadOut -> World w h x y i (max 0 (pred o)) s
        'j' -> loop mode $ case mode of
                          Window -> World w (succ h) x y i o s
                          RadCen -> World w h x (succ y) i o s
                          RadIn  -> World w h x y (succ i) o s
                          RadOut -> World w h x y i (succ o) s
        'k' -> loop mode $ case mode of
                          Window -> World w (max 1 (pred h)) x y i o s
                          RadCen -> World w h x (pred y) i o s
                          RadIn  -> World w h x y (max 0 (pred i)) o s
                          RadOut -> World w h x y i (max 0 (pred o)) s
        'l' -> loop mode $ case mode of
                          Window -> World (succ w) h x y i o s
                          RadCen -> World w h (succ x) y i o s
                          RadIn  -> World w h x y (succ i) o s
                          RadOut -> World w h x y i (succ o) s
        's' -> do
            putStr "string: "
            hSetEcho stdout True
            s' <- getLine
            hSetEcho stdout False
            loop mode $ World w h x y i o s'
        c   -> do
            putStrLn $ show c
            loop mode $ World w h x y i o s

