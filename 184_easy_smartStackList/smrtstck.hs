module SmartStack where

data SortL a = ESort | CSort a (SortL a) deriving Show
data StackL a = EStack | CStack a (StackL a) deriving Show

data SmartStack a = SmartStack { sorted  :: SortL a
                               , stacked :: StackL a } deriving Show

sortPush :: Ord a => SortL a -> a -> SortL a
sortPush ESort n = CSort n ESort
sortPush (CSort h t) n = (max h n) `CSort` (sortPush t (min h n))

stackPush :: StackL a -> a -> StackL a
stackPush EStack n = CStack n EStack
stackPush all@(CStack h t) n = n `CStack` all

