import Data.Monoid (mconcat)
import qualified Data.Set as S (Set, fromList)
import System.Environment (getArgs)

gridCoords :: Int -> Int -> [(Int, Int)]
gridCoords x y = [(x,y) | y <- [0..succ x], x <- [0..succ y]]

findPlants :: Int -> Int -> String -> S.Set (Int, Int)
findPlants w h xs = S.fromList [c | (c,x) <- zip cs xs, x == 'x']
    where cs = gridCoords w h

main = do
    (h:w:r:xs) <- getArgs
    let plants = findPlants (read w) (read h) (mconcat xs)
    print plants

