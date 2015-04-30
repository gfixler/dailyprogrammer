data Tree a b = Branch a (Tree a b) (Tree a b)
              | Tip
              | Leaf b
              deriving (Show)

data Side = T | L | R deriving (Show)
type Crumb a b = (a, Side, Tree a b)
type TreeZipper a b = (Tree a b, Side, [Crumb a b])

zipperTree :: Tree a b -> TreeZipper a b
zipperTree t = (t, T, [])

