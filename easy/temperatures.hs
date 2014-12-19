import System.IO
import Data.Ord
import Data.List
import Control.Monad (when)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    when (n == 0) $ putStrLn "0"
    temps <- getLine
    -- the N temperatures expressed as integers ranging from -273 to 5526
    
    hPutStrLn stderr temps
    
    -- Write answer to stdout
    let result = findCloseToZero $ map (read :: String -> Int) $ words temps
    print result

findCloseToZero :: [Int] -> Int
findCloseToZero temps = if (abs close) `elem` temps then abs close else close
    where close = minimumBy (comparing abs) temps
