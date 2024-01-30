import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (replicateM)


createQueryMap :: [String] -> Map String Int
createQueryMap xs = Map.fromList [(x, 0) | x <- xs]


updateQueryMap :: Map String Int -> [String] -> Map String Int
updateQueryMap queryMap (x:xs) = updateQueryMap (Map.adjust (\x -> x+1) x queryMap) xs
updateQueryMap queryMap [] = queryMap


getOrderedValues :: Map String Int -> [String] -> [Int]
getOrderedValues queryMap queries = [queryMap Map.! q | q <- queries]


main = do
    nStrings <- getLine
    strings <- replicateM (read nStrings) getLine
    nQueries <- getLine
    queries <- replicateM (read nQueries) getLine
    putStrLn $ unlines $ map show $ getOrderedValues (updateQueryMap (createQueryMap queries) strings) queries