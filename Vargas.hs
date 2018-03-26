import System.Environment
import Data.List.Split
import Text.Regex.TDFA

parseCSV :: String -> [(String, String, String)]
parseCSV strs = [y | Just y <- [parseCSVLine x | x <- splitOn "\n" strs]]

parseCSVLine :: String -> Maybe (String, String, String)
parseCSVLine str = parseParse x
    where x = str =~ "^([^,]*),([^,]*),([^,]*)$" :: [[String]]

parseParse :: [[String]] -> Maybe (String, String, String)
parseParse [] = Nothing
parseParse [[]] = Nothing
parseParse [(x:xs)] = Just $ tuplify3(xs)

tuplify3  :: [a] -> (a, a, a)
tuplify3 (x:y:z:[]) = (x, y, z)
tuplify3 _ = error "asdfasdf"

printTuples3 :: [(String, String, String)] -> IO ()
printTuples3 [] = return ()
printTuples3 (t:pls) = do
                        printTuple3 t
                        printTuples3 pls

printTuple3 :: (String, String, String) -> IO ()
printTuple3 (x, y, z) = do
    putStrLn (x ++ "GEKAS" ++ y ++ "GEKAS" ++ z)

main = do
   f <- getArgs
   s <- readFile $ head f
   printTuples3 $ parseCSV s

1/m sum to m (Theta1 * )
-- Xtrain
-- ytrain
-- Xval
-- yval
-- Xtest
-- ytest

function