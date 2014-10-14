module SmartStack where

data SortL a = ESort | CSort a (SortL a) deriving Show

sortPush :: Ord a => SortL a -> a -> SortL a
sortPush ESort n = CSort n ESort
sortPush (CSort head tail) n = higher `CSort` (sortPush tail lower)
    where higher = max head n
          lower = min head n

