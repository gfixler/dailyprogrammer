type Sheet = (Int, Int, Int, Int, Int) -- color, x, y, w, h

onSheet :: Int -> Int -> Sheet -> Bool
onSheet x y (_,l,t,w,h) = x >= l && y >= t && x <= l+w && y <= t+h

