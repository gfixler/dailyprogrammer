import System.IO

main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "How many years have you been alive?"
    age <- getLine
    putStrLn "What is your reddit username?"
    username <- getLine
    let msg = "your name is " ++ name ++ ", you are " ++ age ++ " years old, and your reddit username is " ++ username
        filename = "userinfo.txt"
    putStrLn $ "writing the following message to " ++ filename
    writeFile filename msg -- extra credit
    putStrLn $ msg

