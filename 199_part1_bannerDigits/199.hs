import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Monoid

data Digit = Digit [String] deriving Show

instance Monoid Digit where
    mempty = Digit $ repeat ""
    mappend (Digit a) (Digit b) = Digit $ zipWith (++) a b

digits = transpose $ map (chunksOf 3)
    [" _     _  _     _  _  _  _  _ "
    ,"| |  | _| _||_||_ |_   ||_||_|"
    ,"|_|  ||_  _|  | _||_|  ||_| _|"
    ]

