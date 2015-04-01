clamp :: Ord a => a -> a -> a -> a
clamp l h v = min h (max l v)

