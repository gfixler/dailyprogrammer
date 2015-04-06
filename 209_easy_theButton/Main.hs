import Data.List
import Data.Ord

utpairs = [("UserA", 41.04), ("UserB", 7.06), ("UserC", 20.63), ("UserD", 54.28), ("UserE", 12.59), ("UserF", 31.17), ("UserG", 63.04)]

getFlairs xs = click 0 (sortBy (comparing snd) xs)
    where click _ [] = []
          click o ((u,t):xs) = (u,floor $ 60-(t-o)) : click t xs

