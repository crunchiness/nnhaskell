import System.Environment
import Data.List.Split
import Data.Matrix

parseMatrix :: String -> Matrix Int
parseMatrix str = fromLists [y | Just y <- [parseRow x | x <- splitOn "\n" str]]

parseRow :: String -> Maybe [Int]
parseRow ('#':xs) = Nothing
parseRow []       = Nothing
parseRow str      = Just $ map (read :: String -> Int) $ filter (\x -> x /= []) (splitOn " " str)

main = do
   f <- getArgs
   s <- readFile $ head f
   putStrLn $ prettyMatrix $ parseMatrix s
