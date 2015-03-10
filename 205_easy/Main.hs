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
friendlyDates today t1 t2
    | y1 == y2 && m1 == m2 && d1 == d2 =
        month t1 ++ " " ++ ordinal d1 ++ ", " ++ show y1
    | year == y1 && year == y2 && m1 == m2 =
        month t1 ++ " " ++ ordinal d1 ++ " - " ++ ordinal d2
    | year == y1 && year /= y2 && m1 == m2 && d1 == d2 =
        full t1 y1 m1 d1 ++ " - " ++ full t2 y2 m2 d2
    | y1 == y2 && year /= y1 && year /= y2 =
        month t1 ++ " " ++ ordinal d1 ++ " - " ++ month t2 ++ " " ++ ordinal d2 ++ ", " ++ show y1
    | diffUTCTime t2 t1 >= 31449600 =
        full t1 y1 m1 d1 ++ " - " ++ full t2 y2 m2 d2
    | diffUTCTime t2 t1 < 31449600 =
        month t1 ++ " " ++ ordinal d1 ++ " - " ++ month t2 ++ " " ++ ordinal d2
    where (year,_,_) = ymd today
          (y1,m1,d1) = ymd t1
          (y2,m2,d2) = ymd t2
          full t y m d = month t ++ " " ++ ordinal d ++ ", " ++ show y


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

