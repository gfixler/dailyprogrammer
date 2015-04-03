module Main () where

import Data.List.Zipper ( Zipper(..), fromList
                        , cursor
                        , left, right
                        , insert, push
                        , beginp, endp)

insLeft :: a -> Zipper a -> Zipper a
insLeft x z = if beginp z then left . insert x $ z else left z

pushRight :: a -> Zipper a -> Zipper a
pushRight x z = if endp z then right . push x $ z else right z

type State = String

data TuringMachine a = TM { offset :: Int
                          , states :: [State]
                          , state  :: State
                          , accept :: State
                          , alpha  :: [a]
                          , non    :: a
                          , tape   :: Zipper a
                          } deriving (Show)

machine :: [State] -> State -> State -> [a] -> a -> [a] -> TuringMachine a
machine states state stop alpha non tape = TM 0 states state stop alpha non (fromList tape)

stepL :: TuringMachine a -> TuringMachine a
stepL m = m { tape = insLeft (non m) (tape m) }

stepR :: TuringMachine a -> TuringMachine a
stepR m = m { tape = pushRight (non m) (tape m) }

setState :: State -> TuringMachine a -> TuringMachine a
setState s m = m { state = s }

match :: Eq a => a -> State -> TuringMachine a -> Bool
match x s m = s == state m && x == cursor (tape m)

