import System.IO
import Control.Monad
import Data.Ord
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- The code below will read all the game information for you.
    -- On each game turn, information will be available on the standard input, you will be sent:
    -- -> the total number of visible enemies
    -- -> for each enemy, its name and distance from you
    -- The system will wait for you to write an enemy name on the standard output.
    -- Once you have designated a target:
    -- -> the cannon will shoot
    -- -> the enemies will move
    -- -> new info will be available for you to read on the standard input.
    
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let count = read input_line :: Int -- The number of current enemy ships within range
    
    enemies <- replicateM count $ do
        input_line <- getLine
        let input = words input_line
        let enemy = input!!0 -- The name of this enemy
        let dist = read (input!!1) :: Int -- The distance to your cannon of this enemy
        return (enemy, dist)
    putStrLn $ fst $ head $ sortBy (comparing snd) enemies
    
    loop
