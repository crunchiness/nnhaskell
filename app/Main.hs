module Main where

import System.Environment
import Data.Matrix

import Lib

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    let m = parseMatrix s :: Matrix Double
    putStrLn $ "Rows: " ++ (show $ nrows m)
    putStrLn $ "Cols: " ++ (show $ ncols m)
    putStrLn $ "Min: " ++ (show $ minElem m)
    putStrLn $ "Max: " ++ (show $ maxElem m)
    case squareMatrixVector (getRow 1 m) 32 of
        Nothing  -> putStrLn "Nothing"
        Just mat -> putStrLn $ prettyMatrix mat
    drawSmth
