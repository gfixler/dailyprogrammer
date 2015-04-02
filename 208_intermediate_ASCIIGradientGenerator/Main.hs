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

main = do
    let w = World 44 22 0 0 0 4 " .,;+%#@"
    print w

