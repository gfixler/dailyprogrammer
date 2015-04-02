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

main = do
    wh <- getLine
    let [w', h'] = words wh
        w        = read w' :: Int
        h        = read h' :: Int
    s <- getLine
    c <- getLine
    let cs = words c
    case head cs of
        "radial" -> let [x', y', ri', ro'] = tail cs
                        in do
                            let x = read x' :: Int
                                y = read y' :: Int
                                ri = read ri' :: Int
                                ro = read ro' :: Int
                                img = renderRadial s (fromIntegral w)
                                                     (fromIntegral h)
                                                     (fromIntegral x, fromIntegral y)
                                                     (fromIntegral ri)
                                                     (fromIntegral ro)
                            putStr $ unlines $ img

