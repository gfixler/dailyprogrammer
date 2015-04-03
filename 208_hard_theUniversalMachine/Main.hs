module Main () where

import Data.List.Zipper ( Zipper(..), fromList
                        , left, right
                        , insert, push
                        , start, end
                        , beginp, endp)

type State = String

data TuringMachine a = TM { offset :: Int
                          , states :: [State]
                          , accept :: State
                          , state  :: State
                          , alpha  :: [a]
                          , empty  :: a
                          , init   :: Zipper a
                          } deriving (Show)

machine :: [State] -> State -> State -> [a] -> a -> [a] -> TuringMachine a
machine states state stop alpha empty init = TM 0 states state stop alpha empty (fromList init)

insLeft :: a -> Zipper a -> Zipper a
insLeft x z = if beginp z then left . insert x $ z
                          else left z

pushRight :: a -> Zipper a -> Zipper a
pushRight x z = if endp z then right . push x $ z
                          else right z

