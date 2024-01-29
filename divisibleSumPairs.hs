pairs :: Int -> [(Int, Int)]
pairs n = [(i,j) | i <- [0..n-1], j <- [0..n-1]]


isValidPair :: Int -> [Int] -> (Int, Int) -> Bool
isValidPair k ar (i, j) = i < j && ((ar !! i) + (ar !! j)) `mod` k == 0


solve :: [Int] -> Int -> Int -> Int
solve ar n k = length $ filter (isValidPair k ar) (pairs n)


main :: IO ()
main = do
    nk <- getLine
    ar <- getLine
    let (n:k:_) = map read $ words nk
    print (solve (map read $ words ar) n k)
