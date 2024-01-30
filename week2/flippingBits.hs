import Control.Arrow (ArrowChoice(left))
flipBit :: Int -> Int
flipBit x = if x == 0 then 1 else 0


decToBin :: Int -> [Int]
decToBin 0 = []
decToBin n =  decToBin (n `div` 2) ++ [n `mod` 2]


leftpad :: [Int] -> [Int]
leftpad xs = if length xs == 32 then xs else leftpad (0:xs)


binToDec :: [Int] -> Int
binToDec [] = 0
binToDec (x:xs) = binToDec xs + if x == 1 then 2 ^ length xs else 0


solve :: Int -> Int
solve = binToDec . map flipBit . leftpad . decToBin


main = interact $ unlines . map (show . solve . read) . tail . lines