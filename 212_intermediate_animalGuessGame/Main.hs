data Tree a b = Branch a (Tree a b) (Tree a b)
              | Tip
              | Leaf b
              deriving (Show)

data Side = T | L | R deriving (Show)
type Crumb a b = (a, Side, Tree a b)
type TreeZipper a b = (Tree a b, Side, [Crumb a b])

zipperTree :: Tree a b -> TreeZipper a b
zipperTree t = (t, T, [])

goLeft :: TreeZipper a b -> Maybe (TreeZipper a b)
goLeft ((Branch x l r), s, cs) = Just (l, L, (x, s, r) : cs)
goLeft _ = Nothing

goRight :: TreeZipper a b -> Maybe (TreeZipper a b)
goRight ((Branch x l r), s, cs) = Just (r, R, (x, s, l) : cs)
goRight _ = Nothing

goUp :: TreeZipper a b -> Maybe (TreeZipper a b)
goUp (l, L, ((x,s,r):cs)) = Just (Branch x l r,s,cs)
goUp (r, R, ((x,s,l):cs)) = Just (Branch x l r,s,cs)
goUp _ = Nothing

