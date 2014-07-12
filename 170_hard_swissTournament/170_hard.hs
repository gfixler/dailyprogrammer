import Data.List
import System.Random

players = map (\x -> "Player" ++ show x) [1..32]

rsort players
    | []        = []
    | otherwise = player : rsort (delete player players)
        where player = players !! getStdRandom (randomR (1,length players))
