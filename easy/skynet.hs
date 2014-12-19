import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let r = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let g = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let l = read input_line :: Int -- the length of the landing platform.
    loop r g l

loop r g l = do
    input_line <- getLine
    let s = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let x = read input_line :: Int -- the position on the road of the motorbike.
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    moveBike r g l s x
    
    loop r g l

moveBike r g l s x
  | s > g+1   = putStrLn "SLOW"
  | x > r     = putStrLn "SLOW"
  | s <= g    = putStrLn "SPEED"
  | x + s > r = putStrLn "JUMP"
  | otherwise = putStrLn "WAIT"
