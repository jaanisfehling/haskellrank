getRatio :: (Int -> Bool) -> [Int] -> Int -> Float
getRatio p xs n = fromIntegral (length $ filter p xs) / fromIntegral n


solve :: [Int] -> [Float]
solve (x:xs) =
    [getRatio (\ x -> x > 0) xs x]
    ++ [getRatio (\ x -> x < 0) xs x]
    ++ [getRatio (\ x -> x == 0) xs x]


main = interact $ unlines . map show . solve . map read . words
