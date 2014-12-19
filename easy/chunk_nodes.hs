import System.IO
import Data.Char
import Control.Monad
import Data.Bits

binary7bit :: Char -> String
binary7bit c = map (\x -> if ((ord c) .&. (bit x) == bit x) then '1' else '0') [6,5..0]

binaryString :: String -> String
binaryString = join . map binary7bit

unary_num :: Char -> String
unary_num '0' = "00"
unary_num '1' = "0"
unary_num _ = ""

unary :: Char -> Int -> String -> String
unary mem count [] = (unary_num mem) ++ " " ++ (replicate count '0')
unary mem count (x:xs)
  | count == 0 = (unary x 1 xs)
  | x == mem   = unary mem (count + 1) xs
  | otherwise  = (unary_num mem) ++ " " ++ (replicate count '0')  ++ " " ++ (unary x 1 xs)



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    -- Write answer to stdout
    putStrLn $ unary '0' 0 (binaryString message)
