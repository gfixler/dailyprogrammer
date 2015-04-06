import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Ord (comparing)

getFlairs :: (RealFrac a, Integral b) => [(String, a)] -> [(String, b)]
getFlairs xs = r 0 (sortBy (comparing snd) xs)
    where r _ [] = []
          r p ((u,t):xs) = (u,floor $ 60-(t-p)) : r t xs

parseTime :: String -> (String, Double)
parseTime x = let (u,t) = break (== ' ') x in (u, read t)

formatFlair :: (String, Integer) -> String
formatFlair (u,t) = u ++ " " ++ show t

readUserTime :: IO (String, Double)
readUserTime = do
    l <- getLine
    let user = takeWhile (/= ':') l
        time = reverse . takeWhile (/= ' ') $ reverse l
    return (user, read time :: Double)

formatUserTime :: (String, Integer) -> String
formatUserTime (u,t) = u ++ ": " ++ show t

main = do
    n <- readLn :: IO Int
    userTimes <- replicateM n readUserTime
    putStr . ("\n" ++) . unlines . map formatUserTime $ getFlairs userTimes

