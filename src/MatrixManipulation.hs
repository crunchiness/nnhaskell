module MatrixManipulation (mapMatrix, maxElem, minElem, squareMatrixVector) where

import Data.Matrix
import Data.Word
import qualified Data.Vector as V

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

squareMatrixVector :: V.Vector a -> Int -> Maybe (Matrix a)
squareMatrixVector vec n | n == 0              = Nothing
                         | (length vec) /= n^2 = Nothing
                         | otherwise           = Just m
                         where
                             m = squareMatrixVector' (V.drop n vec) n (rowVector $ V.take n vec)

squareMatrixVector' :: V.Vector a -> Int -> Matrix a -> Matrix a
squareMatrixVector' vec n m | (length vec) == 0 = m
                            | otherwise = squareMatrixVector' (V.drop n vec) n (m <-> rowVector (V.take n vec))

mapMatrix :: Num a => (a -> a) -> Matrix a -> Matrix a
mapMatrix f m = mapMatrix' f m $ nrows m
              where
--                 mapMatrix' :: (a -> b) -> Matrix a -> Int -> Matrix b
              mapMatrix' f m row | row == 1  = mapRow (\_ x -> f x) row m
                                 | otherwise = mapMatrix' f (mapRow (\_ x -> f x) row m) (row - 1)

-- toWord8Range :: Matrix a -> Matrix Word8
-- toWord8Range m = mapMatrix (\x -> max x 255) m'
--                where m' = mapMatrix (\x -> x + (minElem m)) m
