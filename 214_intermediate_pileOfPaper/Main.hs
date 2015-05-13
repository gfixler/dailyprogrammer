type Sheet  = (Int, Int, Int, Int, Int) -- color, x, y, w, h
type Pile   = (Int, Int, Int, Int, [Sheet]) -- l, t, r, b, sheets

onSheet :: Int -> Int -> Sheet -> Bool
onSheet x y (_,l,t,w,h) = x >= l && y >= t && x <= l+w && y <= t+h

newPile :: Int -> Int -> Pile
newPile w h = (0,0,w,h,[])

