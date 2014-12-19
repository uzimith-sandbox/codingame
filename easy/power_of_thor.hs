import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position

    let distX = lx - tx
    let distY = ly - ty
    let directions = walk distX distY
    hPutStrLn stderr $ show $ directions
    loop directions

walk :: Int -> Int -> [String]
walk distX distY = do
    let dx = walkX distX
    let dy = walkY distY
    let rest = gap dx dy
    zipWith (\x y -> x:[y]) dy dx ++ rest

walkX :: Int -> String
walkX distX
  | distX > 0 = replicate (abs distX) 'E'
  | distX < 0 = replicate (abs distX) 'W'
  | otherwise = ""

walkY :: Int -> String
walkY distY
  | distY < 0 = replicate (abs distY) 'N'
  | distY > 0 = replicate (abs distY) 'S'
  | otherwise = ""

gap :: String -> String -> [String]
gap dx dy
  | margin /= 0 = map (\x -> [x]) $ drop same (if margin > 0 then dx else dy)
  | otherwise = []
  where margin  = (length dx) - (length dy)
        same = min (length dx) (length dy)

loop :: [String] -> IO ()
loop [] = return ()
loop (x:xs) = do
    -- input_line <- getLine
    -- let e = read input_line :: Int -- The level of Thor's remaining energy, representing the number of moves he can still make.
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn x
    
    loop xs

