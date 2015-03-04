import qualified Data.Map as M
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

type XYZ = (Int, Int, Int)
data Block = Air | Dirt | Sand | Lava deriving (Show)
type World = (XYZ, M.Map XYZ Block)

newWorld :: XYZ -> World
newWorld (l,w,h) = ((l,w,h), M.fromList $ zip cells (repeat Air))
    where cells = [(x,y,z) | x <- [0..l-1], y <- [0..w-1], z <- [0..h-1]]

