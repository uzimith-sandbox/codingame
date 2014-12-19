import System.IO
import Data.Char
import Control.Monad
import Data.Bits

data Binary = Zero | One deriving (Eq, Show)

binary7bit :: Char -> [Binary]
binary7bit c = map (\x -> if ((ord c) .&. (bit x) == bit x) then One else Zero) [6,5..0]

binaryString :: String -> [Binary]
binaryString = join . map binary7bit

unary :: [Binary] -> String
unary binary = unary' binary Zero 0
  where unaryDigit :: Binary -> String
        unaryDigit Zero = "00"
        unaryDigit One  = "0"
        binary2unary :: Binary -> Int -> String
        binary2unary digit count = (unaryDigit digit) ++ " " ++ (replicate count '0')
        unary' :: [Binary] -> Binary -> Int -> String
        unary' [] digit count     = binary2unary digit count
        unary' (x:xs) digit count
          | count == 0 = unary' xs x 1
          | x == digit = unary' xs digit (count + 1)
          | otherwise  = (binary2unary digit count) ++ " " ++ (unary' xs x 1)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    -- Write answer to stdout
    putStrLn . unary . binaryString $ message
