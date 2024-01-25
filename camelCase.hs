import Data.List
import qualified Data.Char as Char

split :: String -> [String] -> String -> [String]
split str names currentName | str == "" || str == "()" = names ++ [currentName]
                            | Char.isUpper (head str) && (currentName /= "") = split str (names ++ [currentName]) ""
                            | otherwise = split (tail str) names (currentName ++ [Char.toLower $ head str])


toPascalCase :: [String] -> String
toPascalCase = concatMap (\(x:xs) -> Char.toUpper x : xs)


combine :: [String] -> String -> String
combine (x:xs) method | method == "C" = toPascalCase (x:xs)
                      | method == "V" = x ++ toPascalCase xs
                      | otherwise = x ++ toPascalCase xs ++ "()"


_splitBySemicolon :: String -> [String] -> String -> [String]
_splitBySemicolon str xs current | str == "" = xs ++ [current]
                                 | head str == ';' = _splitBySemicolon (tail str) (xs ++ [current]) ""
                                 | otherwise = _splitBySemicolon (tail str) xs (current ++ [head str])


splitBySemicolon :: String -> [String]
splitBySemicolon s = _splitBySemicolon s [] ""


solve :: [String] -> String
solve xs | head xs == "S" = unwords $ split (xs !! 2) [] ""
         | otherwise = combine (words (xs !! 2)) (xs !! 1)


main :: IO ()
main = do 
    x <- getContents
    putStrLn $ (unlines . map (solve . splitBySemicolon) . lines) x
