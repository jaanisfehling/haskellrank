solve :: Int -> Int -> Int -> Int -> [Int] -> [Int]
solve least most leastBroken mostBroken [] = [mostBroken, leastBroken]
solve least most leastBroken mostBroken (x:xs) | x > most = solve least x leastBroken (mostBroken+1) xs
                                               | x < least = solve x most (leastBroken+1) mostBroken xs
                                               | otherwise = solve least most leastBroken mostBroken xs
                                               
                                               
solveWrapper :: [Int] -> [Int]
solveWrapper xs = solve (head xs) (head xs) 0 0 xs

main = interact $ unwords . map show . solveWrapper . map read . words . head . tail . lines
