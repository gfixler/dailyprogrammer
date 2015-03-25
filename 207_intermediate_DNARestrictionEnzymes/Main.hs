import Data.Char (toLower)
import System.Environment (getArgs)
import System.IO (readFile)

main = do
    [file] <- getArgs
    contents <- readFile file
    let code = filter (`elem` "gcta") $ map toLower contents
    putStrLn code

