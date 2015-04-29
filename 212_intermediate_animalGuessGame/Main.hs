data Qs = E | Q String Qs Qs | A String deriving (Show)
data Next = Narrow | Guess | Fail

next :: Qs -> String
next E = "You've stumped me!"
next (A s) = "My guess: " ++ s ++ "!"
next (Q s y n) = s

