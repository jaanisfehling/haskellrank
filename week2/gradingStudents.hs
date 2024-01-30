roundGrade :: Int -> Int
roundGrade g = if diff < 3
          then g + diff
          else g
          where diff = 5 - g `mod` 5


solve :: Int -> Int
solve g = if g < 38 then g else roundGrade g


main = interact $ unlines . map (show . solve . read) . tail . lines