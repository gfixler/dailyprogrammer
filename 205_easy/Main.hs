import Control.Monad        (when)
import Data.Time.Clock      (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar   (toGregorian)
import Data.Time.Format     (parseTime)
import System.Environment   (getArgs)
import System.Locale        (defaultTimeLocale)
import System.Exit          (exitFailure)

ymd :: UTCTime -> (Integer,Int,Int)
ymd = toGregorian . utctDay

parseFDate :: String -> Maybe UTCTime
-- converts "%F"-format date string ("YYYY-MM-DD")
parseFDate d = parseTime defaultTimeLocale "%F" d

main = do
    args <- getArgs
    when (length args /= 2) handleBadArgs
    let [a, b] = args
    t1 <- readDateOrDie a
    t2 <- readDateOrDie b
    when (compare t1 t2 == GT) handleBadDateOrder
    return ()

readDateOrDie :: String -> IO UTCTime
readDateOrDie d = case parseFDate d of
    Just t1 -> return t1
    Nothing -> handleBadArgs

handleBadArgs :: IO a
handleBadArgs = do
    putStrLn "Must pass in 2 dates in YYYY-MM-DD format"
    exitFailure

handleBadDateOrder :: IO a
handleBadDateOrder = do
    putStrLn "Dates must be passed in order (earlier one first)"
    exitFailure

