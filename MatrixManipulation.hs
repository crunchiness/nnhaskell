module MatrixManipulation (maxElem, minElem) where

import Data.Matrix

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
