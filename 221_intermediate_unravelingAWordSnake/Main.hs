import qualified Data.Map as M

type SnakeMap = M.Map (Int,Int) Char

enum :: [a] -> [(Int,a)]
enum = zip [0..]

numLines :: String -> [(Int,String)]
numLines = enum . lines

numLineChars :: (Int,String) -> [((Int,Int),Char)]
numLineChars (y,s) = map (\(x,c) -> ((x,y),c)) (enum s)

toSnakeMap :: String -> SnakeMap
toSnakeMap = M.fromList . nospaces . concatMap numLineChars . numLines
    where nospaces = filter ((/=' ') . snd)

left, right :: (Int, Int) -> (Int, Int)
left x = case x of (1,0) -> (0,1)
                   (0,1) -> (-1,0)
                   (-1,0) -> (0,-1)
                   (0,-1) -> (1,0)

right = left . left . left

-- popSnakeWord :: SnakeMap -> (Int,Int) -> (String, (Int,Int), SnakeMap)
-- popSnakeWord m (x,y) = map (flip M.lookup m)

main = print (toSnakeMap example)

example = "SHENANIGANS       DOUBLET\n          A       N     E\n          L       U     R\n          T       O     A\n          YOUNGSTER     B\n                        Y\n                        T\n                  ECNESSE\n"

