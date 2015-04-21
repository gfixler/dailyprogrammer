import Data.Char (toLower)

bfm :: Char -> String -> String
bfm c (x:xs) | c == x = c:'o':'-':xs
             | x `elem` "AEIOU" = c:'o':'-':c:toLower x:xs
bfm c (_:xs) = toLower c:'o':' ':c:xs

game :: String -> String
game name = concat [name,", ",name," ",bfm 'B' name,",\nBonana fanna "
                   ,bfm 'F' name,",\nFee fy ",bfm 'M' name,",\n",name,"!"]

main = getLine >>= putStrLn . game . init

