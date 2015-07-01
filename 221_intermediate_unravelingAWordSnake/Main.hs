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

