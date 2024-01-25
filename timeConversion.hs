firstTwoAsInt :: String -> Int
firstTwoAsInt s = read $ take 2 s

leftpad :: Int -> String
leftpad x = if x >= 10
    then show x
    else "0" ++ show x

fixPM :: String -> String
fixPM s = if firstTwoAsInt s < 12
    then leftpad (firstTwoAsInt s + 12) ++ drop 2 s
    else s

fixAM :: String -> String
fixAM s = if firstTwoAsInt s >= 12
    then leftpad (firstTwoAsInt s - 12) ++ drop 2 s
    else s

solve :: String -> String
solve s = if drop 8 s == "PM"
    then fixPM $ take 8 s
    else fixAM $ take 8 s

main = interact solve
