import Data.Char (toLower)

bfm :: Char -> String -> String
bfm c (x:xs) | c == x = c:'o':'-':xs
             | x `elem` "AEIOU" = c:'o':'-':c:toLower x:xs
bfm c (_:xs) = toLower c:'o':' ':c:xs

game :: String -> String
game name = concat $ zipWith (++) names words
    where words = [", "," ",",\nBonana fanna ",",\nFee fy ",",\n","!"]
          names = map (flip ($) name) [id,id,bfm 'B',bfm 'F',bfm 'M',id]

main = getLine >>= putStrLn . game . init

