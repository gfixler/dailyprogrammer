import Data.List
import System.Environment

type Event = (Int, String)

eventToStr :: Event -> String
eventToStr (time,task) = show time ++ " " ++ task

strToEvent :: String -> Event
strToEvent eventStr = (time, task)
    where time = (read $ takeWhile (/=' ') eventStr) :: Int
          task = tail $ dropWhile (/=' ') eventStr

printEvents :: [Event] -> IO ()
printEvents = putStrLn . unlines . map eventToStr . sort

getNewEvent :: IO Event
getNewEvent = do
    putStrLn "Task? (e.g. Do laundry)"
    task <- getLine
    putStrLn "Time? (e.g. 0, 445, 1115, etc)"
    timeStr <- getLine
    let time = read timeStr :: Int
    return (time, task)

loadEvents :: String -> IO [Event]
loadEvents filename = do
    contents <- readFile filename
    return $ map strToEvent (lines contents)

saveEvents :: String -> [Event] -> IO ()
saveEvents filename events = do
    let contents = unlines $ map eventToStr events
    writeFile filename contents

addEvent :: IO ()
addEvent = do
    event <- getNewEvent
    return ()

main = do
    args <- getArgs
    let filename = head args
    contents <- loadEvents filename
    printEvents contents
    putStrLn "(A)dd\n(E)dit\n(R)emove task\n(Enter to quit)"
    choice <- getLine
    case choice of
        "a" -> addEvent
        "b" -> return ()
        "c" -> return ()
        _   -> return ()
    return ()

    -- contents <- loadEvents "test"
    -- saveEvents "output" contents

