data Facing = N | S | E | W      deriving (Show)
data Cmd    = Fwd | TL | TR      deriving (Show)
data Bot    = Bot Int Int Facing deriving (Show)

step :: Bot -> Char -> Bot
step (Bot x y d) c = Bot x' y' d'
    where (x',y',d') = case (d,c) of
                           (N,'S') -> (x,(y+1),N)
                           (S,'S') -> (x,(y-1),S)
                           (E,'S') -> ((x+1),y,E)
                           (W,'S') -> ((x-1),y,W)
                           (N,'L') -> (x,y,W)
                           (S,'L') -> (x,y,E)
                           (E,'L') -> (x,y,N)
                           (W,'L') -> (x,y,S)
                           (N,'R') -> (x,y,E)
                           (S,'R') -> (x,y,W)
                           (E,'R') -> (x,y,S)
                           (W,'R') -> (x,y,N)
                           _       -> (x,y,d)

