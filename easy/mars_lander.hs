import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of points used to draw the surface of Mars.
    
    locations <- replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let land_x = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
        let land_y = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return (land_x, land_y)
    hPutStrLn stderr $ show locations
    loop $ head locations

loop location = do
    input_line <- getLine
    let input = words input_line
    let x = read (input!!0) :: Int
    let y = read (input!!1) :: Int
    let hs = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
    let vs = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
    let f = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
    let r = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
    let p = read (input!!6) :: Int -- the thrust power (0 to 4).
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- R P. R is the desired rotation angle. P is the desired thrust power.
    moveLander location x y hs vs f r p
    loop location

moveLander (land_x, land_y) x y hs vs f r p
  | vs <  10  = putStrLn "0 0"
  | vs < -50  = putStrLn "0 4"
  | otherwise = putStrLn "0 0"
