module Parse (parseTransition, parse) where

import Data.List.Zipper (Zipper(..), fromList)
import Turing (machine, Direction(L,R), Transition, TuringMachine)

parseTransition :: String -> Either String (Transition Char)
parseTransition s = case words s of
                        [s1,[c1],_,s2,[c2],"<"] -> Right ((s1,c1),(s2,c2),L)
                        [s1,[c1],_,s2,[c2],">"] -> Right ((s1,c1),(s2,c2),R)
                        _                   -> Left s

parse :: (Monad m, Functor m) => m String -> m (TuringMachine Char)
parse s = do
    d <- fmap lines s
    let [alpha, states, state, accept, init] = take 5 d
        trans = map parseTransition $ drop 5 d
    return $ machine (words states) state accept trans alpha '_' init

