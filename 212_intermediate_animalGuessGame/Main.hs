data Tree a b = Branch a (Tree a b) (Tree a b)
              | Tip
              | Leaf b
              deriving (Show)

data TreeZipper a b = Top (Tree a b)
                    | L (Tree a b) a (Tree a b)
                    | R (Tree a b) a (Tree a b)
                    | Bottom (Tree a b) a (Tree a b)
                    deriving (Show)


