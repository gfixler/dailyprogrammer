boxes :: Int -> (Int, Int)
boxes x = head $ reverse [(m,n) | m <- [1..x], n <- [m..x], m*n == x]

box :: Int -> (Int, Int)
box x = head $ reverse [(m,n) | n <- [1..x], m <- [n..x], m*n == x]

