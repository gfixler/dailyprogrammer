import System.Environment

dnaPair :: Char -> Char
dnaPair c = case c of
                'A' -> 'T'
                'T' -> 'A'
                'C' -> 'G'
                'G' -> 'C'

main = do
    [d] <- getArgs
    putStrLn d
    putStrLn $ fmap dnaPair d

