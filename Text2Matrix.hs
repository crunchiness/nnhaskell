module Text2Matrix where

import Data.List.Split
import Data.Matrix

parseMatrix :: Read a => String -> Matrix a
parseMatrix str = fromLists [y | Just y <- [parseRow x | x <- splitOn "\n" str]]

parseRow :: Read a => String -> Maybe [a]
parseRow ('#':_) = Nothing
parseRow []      = Nothing
parseRow str     = Just $ map read $ filter (\x -> x /= []) (splitOn " " str)
