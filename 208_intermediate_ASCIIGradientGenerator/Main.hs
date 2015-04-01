clamp :: Ord a => a -> a -> a -> a
clamp l h v = min h (max l v)

hypot2D :: Floating a => a -> a -> a -> a -> a
hypot2D x1 y1 x2 y2 = sqrt (x^2 + y^2)
    where x = x2 - x1
          y = y2 - y1

radGrad :: (Floating a, Ord a) => a -> a -> a -> a -> a -> a -> a
radGrad cx cy ri ro x y = clamp ri ro d
    where d = hypot2D cx cy x y

