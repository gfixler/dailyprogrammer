import Data.List (transpose)
import System.IO (getContents)

main = do
    xs <- fmap (map words . lines) getContents
    let ns = map (flip replicate "") [0..]
        ys = map (filter (not . null)) $ transpose $ zipWith (++) ns xs
        zs = unlines $ map unwords ys
    putStrLn zs

