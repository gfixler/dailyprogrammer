import Data.List

hexCharVal c = length $ takeWhile (/= c) "0123456789ABCDEF"

hexDots c = ["    ","   *","  * ","  **",
             " *  "," * *"," ** "," ***",
             "*   ","*  *","* * ","* **",
             "**  ","** *","*** ","****"] !! (hexCharVal c)

renderHexPair s = (hexDots $ head s) ++ (hexDots $ head (tail s))
renderHexPic s = unlines (map renderHexPair $ words s)
showHexPic = putStr . renderHexPic

pic1 = "FF 81 BD A5 A5 BD 81 FF"
pic2 = "AA 55 AA 55 AA 55 AA 55"
pic3 = "3E 7F FC F8 F8 FC 7F 3E"
pic4 = "93 93 93 F3 F3 93 93 93"

