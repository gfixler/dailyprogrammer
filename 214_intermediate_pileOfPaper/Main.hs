type Sheet  = (Int, Int, Int, Int, Int) -- color, x, y, w, h
type Pile   = (Int, Int, Int, Int, [Sheet]) -- l, t, r, b, sheets

onSheet :: Sheet -> Int -> Int -> Bool
onSheet (_,l,t,w,h) x y = x >= l && y >= t && x <= l+w-1 && y <= t+h-1

newPile :: Int -> Int -> Pile
newPile w h = (0,0,w,h,[])

pileOn :: Sheet -> Pile -> Pile
pileOn s@(c,x,y,w,h) (l,t,r,b,p) =
        (min x l, min y t, max (x+w) r, max (y+h) b, s:p)

