readLinesOfInts :: String -> [Int]
readLinesOfInts = concat . map (map read . words) . lines

main = undefined

