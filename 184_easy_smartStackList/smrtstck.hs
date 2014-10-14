module SmartStack where

data SortL a = ESort | CSort a (SortL a) deriving Show

sortPush :: Ord a => SortL a -> a -> SortL a
sortPush ESort n = CSort n ESort
sortPush (CSort h t) n = (max h n) `CSort` (sortPush t (min h n))

