boxes :: Int -> (Int, Int)
boxes x = head $ reverse [(m,n) | m <- [1..x], n <- [m..x], m*n == x]

