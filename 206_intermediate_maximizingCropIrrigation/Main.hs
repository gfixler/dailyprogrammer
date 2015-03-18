import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment

findPlants :: [String] -> S.Set (Int, Int)
findPlants rs = S.fromList [(x,y) | (cs,y) <- zip rs [0..], (p,x) <- zip cs [0..], p == 'x']

main = do
    (h:w:r:rs) <- getArgs
    print $ findPlants rs

