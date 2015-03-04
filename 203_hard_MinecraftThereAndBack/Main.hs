import qualified Data.Map as M
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

data Block = Air | Dirt | Sand | Lava deriving (Show)

