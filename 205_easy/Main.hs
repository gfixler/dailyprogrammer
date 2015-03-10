import Control.Monad        (when)
import Data.Time.Clock      (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar   (toGregorian)
import Data.Time.Format     (formatTime, parseTime)
import System.Environment   (getArgs)
import System.Locale        (defaultTimeLocale)
import System.Exit          (exitFailure)

ymd :: UTCTime -> (Integer,Int,Int)
ymd = toGregorian . utctDay

parseFDate :: String -> Maybe UTCTime
-- converts "%F"-format date string ("YYYY-MM-DD")
parseFDate t = parseTime defaultTimeLocale "%F" t

month :: UTCTime -> String
month t = formatTime defaultTimeLocale "%B" t

ordinalSuffix :: Int -> String
ordinalSuffix n | n > 10 && n < 20 = "th"
ordinalSuffix n = case n `mod` 10 of
                      1 -> "st"
                      2 -> "nd"
                      3 -> "rd"
                      _ -> "th"

ordinal :: Int -> String
ordinal n = show n ++ ordinalSuffix n

main = do
    args <- getArgs
    when (length args /= 2) dieBadArgs
    let [a, b] = args
    t1 <- readDateOrDie a
    t2 <- readDateOrDie b
    when (compare t1 t2 == GT) dieBadDateOrder
    today <- getCurrentTime
    putStrLn $ friendlyDates today t1 t2
    return ()

friendlyDates :: UTCTime -> UTCTime -> UTCTime -> String
friendlyDates today t1 t2 = "temp"

readDateOrDie :: String -> IO UTCTime
readDateOrDie d = case parseFDate d of
    Just t1 -> return t1
    Nothing -> dieBadArgs

dieBadArgs :: IO a
dieBadArgs = do
    putStrLn "Must pass in 2 dates in YYYY-MM-DD format"
    exitFailure

dieBadDateOrder :: IO a
dieBadDateOrder = do
    putStrLn "Dates must be passed in order (earlier one first)"
    exitFailure

