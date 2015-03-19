import Data.Monoid (mconcat)
import qualified Data.Set as S (Set, fromList)
import System.Environment (getArgs)

hypot :: Floating a => a -> a -> a
hypot x y = sqrt $ x * x + y * y

circWidths :: Int -> [Int]
circWidths r = r : width r 1
    where width x y | x == 0    = []
                    | h >= r    = width (pred x) y
                    | otherwise = x : width x (succ y)
                    where
                        h = floor $ hypot (fromIntegral x) (fromIntegral y)

gridPoints :: Int -> Int -> [(Int, Int)]
gridPoints x y = [(x,y) | y <- [0..succ x], x <- [0..succ y]]

findPlants :: Int -> Int -> String -> S.Set (Int, Int)
findPlants w h xs = S.fromList [c | (c,x) <- zip cs xs, x == 'x']
    where cs = gridPoints w h

main = do
    (h:w:r:xs) <- getArgs
    let plants = findPlants (read w) (read h) (mconcat xs)
    print plants

