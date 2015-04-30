data Tree a b = Branch a (Tree a b) (Tree a b)
              | Tip
              | Leaf b
              deriving (Show)


