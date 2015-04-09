import System.Random (getStdRandom, randomR)

choose :: [a] -> IO a
choose xs = do
    i <- getStdRandom (randomR (0, length xs - 1))
    return $ xs !! i

data Corner    = TL | TR | BL | BR deriving (Eq, Show)
data Expansion = H | V deriving (Eq, Show)

