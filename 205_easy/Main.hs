import Control.Monad        (when)
import Data.Time.Clock      (getCurrentTime, diffUTCTime, utctDay, UTCTime)
import Data.Time.Calendar   (toGregorian)
import Data.Time.Format     (formatTime, readTime, parseTime)
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
    | exactSameDay  = full t1 y1 m1 d2
    | monthThisYear = monthDay t1 d1   ++ " - " ++ ordinal d2
    | inFutureYear  = monthDay t1 d1   ++ " - " ++ full t2 y2 m2 d2
    | atLeastAYear  = full t1 y1 m1 d1 ++ " - " ++ full t2 y2 m2 d2
    | notQuiteAYear = monthDay t1 d1   ++ " - " ++ monthDay t2 d2
    where (yr,_,_)      = ymd today
          (y1,m1,d1)    = ymd t1
          (y2,m2,d2)    = ymd t2
          yearInSeconds = 31449600 -- actually 364*24*60*60
          exactSameDay  = y1 == y2 && m1 == m2 && d1 == d2
          monthThisYear = yr == y1 && yr == y2 && m1 == m2
          inFutureYear  = y1 == y2 && yr /= y1 && yr /= y2
          atLeastAYear  = diffUTCTime t2 t1 >= yearInSeconds
          notQuiteAYear = diffUTCTime t2 t1 < yearInSeconds
          full t y m d  = month t ++ " " ++ ordinal d ++ ", " ++ show y
          monthDay t d  = month t ++ " " ++ ordinal d

testFriendlyDates :: Bool
testFriendlyDates =
    get "2015-07-01" "2015-07-04" == "July 1st - 4th"
    && get "2015-12-01" "2016-02-03" == "December 1st - February 3rd"
    && get "2015-12-01" "2017-02-03" == "December 1st, 2015 - February 3rd, 2017"
    && get "2016-03-01" "2016-05-05" == "March 1st - May 5th, 2016"
    && get "2017-01-01" "2017-01-01" == "January 1st, 2017"
    && get "2022-09-05" "2023-09-04" == "September 5th, 2022 - September 4th, 2023"
    where convert t = readTime defaultTimeLocale "%F" t :: UTCTime
          today     = convert "2015-03-09" -- fix day against tests
          get t1 t2 = friendlyDates today (convert t1) (convert t2)

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

