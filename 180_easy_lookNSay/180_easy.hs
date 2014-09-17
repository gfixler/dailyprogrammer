import Data.List


-- converts runs of chars in string to len + char
-- e.g. "112223" => "213213" (i.e. "2 1s, 3 2s, 1 3")
lookNSay :: String -> String
lookNSay s = concatMap unpair $ zip lengs heads
    where heads = map ((:[]) . head) $ group s
          lengs = map (show . length) $ group s
          unpair (a,b) = a ++ b

