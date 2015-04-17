data Facing = N | S | E | W      deriving (Show)
data Cmd    = Fwd | TL | TR      deriving (Show)
data Bot    = Bot Int Int Facing deriving (Show)

parseCommands :: String -> [Cmd]
parseCommands = map parseCommand
    where parseCommand x = case x of 'S' -> Fwd
                                     'L' -> TL
                                     'R' -> TR

