module Main () where

import Data.List.Zipper ( Zipper(..), fromList
                        , cursor, replace
                        , left, right
                        , insert, push
                        , beginp, endp)

insLeft :: a -> Zipper a -> Zipper a
insLeft x z = if beginp z then left . insert x $ z else left z

pushRight :: a -> Zipper a -> Zipper a
pushRight x z = if endp z then right . push x $ z else right z

type State = String
type StateCell a = (State, a)
data Direction = Left | Right deriving (Show)
type Transition a = (StateCell a, StateCell a, Direction)

data TuringMachine a = TM { offset :: Int
                          , states :: [State]
                          , state  :: State
                          , accept :: State
                          , trans  :: [Transition a]
                          , alpha  :: [a]
                          , non    :: a
                          , tape   :: Zipper a
                          } deriving (Show)

machine :: [State] -> State -> State -> [Transition a] -> [a] -> a -> [a] -> TuringMachine a
machine states state stop trans alpha non tape = TM 0 states state stop trans alpha non (fromList tape)

stepL :: TuringMachine a -> TuringMachine a
stepL m = m { tape = insLeft (non m) (tape m) }

stepR :: TuringMachine a -> TuringMachine a
stepR m = m { tape = pushRight (non m) (tape m) }

getStateCell :: TuringMachine a -> StateCell a
getStateCell m = (state m, cursor (tape m))

setStateCell :: StateCell a -> TuringMachine a -> TuringMachine a
setStateCell (s, x) m = m { tape = replace x (tape m), state = s }

match :: Eq a => StateCell a -> TuringMachine a -> Bool
match (s, x) m = s == state m && x == cursor (tape m)

