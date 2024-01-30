import Data.Set (Set)
import qualified Data.Set as Set


findUnique :: Set Int -> [Int] -> Int
findUnique found (x:xs) = if Set.member x found 
                          then findUnique (Set.delete x found) xs
                          else findUnique (Set.insert x found) xs
findUnique found [] = Set.elemAt 0 found


main = interact $ show . findUnique Set.empty . map read . words .last . lines