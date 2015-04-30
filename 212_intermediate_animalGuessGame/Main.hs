data Tree a b = E | B a (Tree a b) (Tree a b) | T b deriving (Show)

