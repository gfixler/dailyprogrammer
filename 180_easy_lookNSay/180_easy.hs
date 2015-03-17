import Data.List


-- converts runs of chars in string to len + char
-- e.g. "112223" => "213213" (i.e. "2 1s, 3 2s, 1 3")
lookNSay :: String -> String
lookNSay = concatMap foo . group
    where foo = \x -> (show $ length x) ++ [head x]

