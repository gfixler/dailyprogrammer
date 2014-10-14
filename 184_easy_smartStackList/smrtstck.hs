module SmartStack where

data SortL a = ESort | CSort a (SortL a) deriving Show
data StackL a = EStack | CStack a (StackL a) deriving Show

data SmartStack a = SmartStack { sorted  :: SortL a
                               , stacked :: StackL a } deriving Show

intSmartStack :: SmartStack Int
intSmartStack = SmartStack { sorted  = ESort  :: SortL Int
                           , stacked = EStack :: StackL Int }

sortPush :: Ord a => SortL a -> a -> SortL a
sortPush ESort n = CSort n ESort
sortPush (CSort h t) n = (max h n) `CSort` (sortPush t (min h n))

stackPush :: StackL a -> a -> StackL a
stackPush EStack n = CStack n EStack
stackPush all@(CStack h t) n = n `CStack` all

push :: Ord a => SmartStack a -> a -> SmartStack a
push s n = SmartStack { sorted = newsort, stacked = newstack }
    where newsort  = sortPush (sorted s) n
          newstack = stackPush (stacked s) n

stackPop :: StackL a -> StackL a
stackPop EStack = EStack
stackPop (head `CStack` tail) = tail

sortDel :: Eq a => SortL a -> a -> SortL a
sortDel ESort _ = ESort
sortDel (head `CSort` tail) n
    | head == n = tail
    | otherwise = head `CSort` (sortDel tail n)

