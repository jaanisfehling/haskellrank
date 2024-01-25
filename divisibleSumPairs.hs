cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


isValidPair :: Int -> (Int, Int) -> Bool
isValidPair k (i, j) = i < j && (i + j) `mod` k == 0


solve :: [Int] -> Int -> Int
solve xs k = length $ filter (isValidPair k) (cartProd xs xs)


main :: IO ()
main = do
    nk <- getLine
    arr <- getLine
    print (solve (map read $ words arr) (read $ last $ words nk))
