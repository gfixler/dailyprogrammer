import qualified Data.Map as M (Map, fromList, keys, findWithDefault)

type CharPad = M.Map (Int, Int) Char

enum :: [a] -> [(Int, a)]
enum = zip [0..]

numLines :: String -> [(Int, String)]
numLines = reverse . enum . reverse . lines

numLineChars :: (Int, String) -> [((Int, Int), Char)]
numLineChars (y, s) = map (\(x,c) -> ((x,y),c)) (enum s)

charGrid :: String -> [((Int, Int), Char)]
charGrid = concatMap numLineChars . numLines

charGridBounds :: CharPad -> ((Int, Int), (Int, Int))
charGridBounds = foldr f e . M.keys
    where e = ((maxBound, maxBound), (minBound, minBound))
          f = \(x, y) ((xl, yl), (xh, yh)) -> ((min x xl, min y yl), (max x xh, max y yh))

charAtCoord :: (Int, Int) -> CharPad -> Char
charAtCoord = M.findWithDefault ' '

renderCharPad p = unlines [[charAtCoord (x, y) p | y <- [yl .. yh]] | x <- [xl .. xh]]
    where ((xl, yl), (xh, yh)) = charGridBounds p

