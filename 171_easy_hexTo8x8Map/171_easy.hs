import Data.List

str = foldl (++) ""
smooshText s = str $ words s

hexCharVal c = length $ takeWhile (/= c) "0123456789ABCDEF"
hexDots c = ["    ","   *","  * ","  **",
             " *  "," * *"," ** "," ***",
             "*   ","*  *","* * ","* **",
             "**  ","** *","*** ","****"] !! (hexCharVal c)
renderHexLine s = str (map hexDots $ smooshText s)
renderHex s = str $ intersperse "\n" (map renderHexLine $ lines s)

pic1 = "FF 81\nBD A5\nA5 BD\n81 FF"
pic2 = "AA 55\nAA 55\nAA 55\nAA 55"
pic3 = "3E 7F\nFC F8\nF8 FC\n7F 3E"
pic4 = "93 93\n93 F3\nF3 93\n93 93"

