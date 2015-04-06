import Control.Monad (forM)
import Data.List (sortBy)
import Data.Ord (comparing)

getFlairs :: (RealFrac a, Integral b) => [(String, a)] -> [(String, b)]
getFlairs xs = r 0 (sortBy (comparing snd) xs)
    where r _ [] = []
          r p ((u,t):xs) = (u,floor $ 60-(t-p)) : r t xs

main = do
    n <- getLine
    let n' = read n :: Int
    lines <- forM [1..n'] (\x -> getLine)
    print lines

