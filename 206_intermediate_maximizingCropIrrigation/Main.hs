import qualified Data.Set as S
import System.Environment

gridCoords :: Int -> Int -> [(Int, Int)]
gridCoords x y = [(x,y) | y <- [0..succ x], x <- [0..succ y]]

findPlants :: [String] -> S.Set (Int, Int)
findPlants rs = S.fromList [(x,y) | (cs,y) <- zip rs [0..], (p,x) <- zip cs [0..], p == 'x']

main = do
    (h:w:r:rs) <- getArgs
    print $ findPlants rs

