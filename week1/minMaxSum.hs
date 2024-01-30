import Data.List

minSum :: [Int] -> Int
minSum xs = sum $ delete (maximum xs) xs

maxSum :: [Int] -> Int
maxSum xs = sum $ delete (minimum xs) xs

solve :: [Int] -> [Int]
solve xs = [minSum xs, maxSum xs]


main = interact $ unwords . map show . solve . map read . words
