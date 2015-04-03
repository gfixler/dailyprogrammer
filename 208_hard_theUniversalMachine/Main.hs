module Main () where

import Data.List.Zipper ( Zipper(..), fromList
                        , left, right
                        , insert, push
                        , start, end
                        , beginp, endp)

type State = String

data TuringMachine a = TM Int [a] [State] State State (Zipper a) deriving (Show)

machine :: [a] -> [State] -> State -> State -> [a] -> TuringMachine a
machine alpha states start stop init = TM 0 alpha states start stop (fromList init)

insLeft :: a -> Zipper a -> Zipper a
insLeft x z = if beginp z then left . insert x $ z
                          else left z

pushRight :: a -> Zipper a -> Zipper a
pushRight x z = if endp z then right . push x $ z
                          else right z

