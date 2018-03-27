module Main where

import System.Environment
import Data.Matrix

import MatrixManipulation
import Text2Matrix

main :: IO ()
main = do
    f <- getArgs
    s <- readFile $ head f
    let m = parseMatrix s :: Matrix Int
    putStrLn $ "Rows: " ++ (show $ nrows m)
    putStrLn $ "Cols: " ++ (show $ ncols m)
    putStrLn $ "Min: " ++ (show $ minElem m)
    putStrLn $ "Max: " ++ (show $ maxElem m)
