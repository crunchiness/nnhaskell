import System.Environment
import Data.List.Split
import Data.Matrix

parseMatrix :: Read a => String -> Matrix a
parseMatrix str = fromLists [y | Just y <- [parseRow x | x <- splitOn "\n" str]]

parseRow :: Read a => String -> Maybe [a]
parseRow ('#':_) = Nothing
parseRow []      = Nothing
parseRow str     = Just $ map read $ filter (\x -> x /= []) (splitOn " " str)

maxElem :: Ord a => Matrix a -> a
maxElem m = mostElemFrom (>) m rows cols lastElem
            where
                lastElem = m ! (rows, cols)
                rows = nrows m
                cols = ncols m

minElem :: Ord a => Matrix a -> a
minElem m = mostElemFrom (<) m rows cols lastElem
            where
                lastElem = m ! (rows, cols)
                rows = nrows m
                cols = ncols m

mostElemFrom :: Ord a => (a -> a -> Bool) -> Matrix a -> Int -> Int -> a -> a
mostElemFrom cmp m i j cmost | i == 0      = cmost
                             | j == 0      = mostElemFrom cmp m (i - 1) cols cmost
                             | cmp c cmost = mostElemFrom cmp m i (j - 1) c
                             | otherwise   = mostElemFrom cmp m i (j - 1) cmost
                             where
                                 c = m ! (i, j)
                                 cols = ncols m

main :: IO ()
main = do
    f <- getArgs
    s <- readFile $ head f
    let m = parseMatrix s :: Matrix Int
    putStrLn $ "Rows: " ++ (show $ nrows m)
    putStrLn $ "Cols: " ++ (show $ ncols m)
    putStrLn $ "Min: " ++ (show $ minElem m)
    putStrLn $ "Max: " ++ (show $ maxElem m)
