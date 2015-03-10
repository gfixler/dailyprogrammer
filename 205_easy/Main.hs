import Control.Monad        (when)
import Data.Time.Clock      (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar   (toGregorian)
import Data.Time.Format     (readTime)
import System.Environment   (getArgs)
import System.Locale        (defaultTimeLocale)
import System.Exit          (exitFailure)

ymd :: UTCTime -> (Integer,Int,Int)
ymd = toGregorian . utctDay

parseFDate :: String -> UTCTime
-- converts "%F"-format date string ("YYYY-MM-DD")
parseFDate d = readTime defaultTimeLocale "%F" d

main = do
    args <- getArgs
    when (length args /= 2) handleBadArgs
    return ()

handleBadArgs :: IO a
handleBadArgs = do
    putStrLn "Must pass in 2 dates in YYYY-MM-DD format"
    exitFailure

