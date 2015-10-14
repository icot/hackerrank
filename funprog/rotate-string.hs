import Control.Monad

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

rotations s = rotations' s (length s) []
    where
        rotations' s count acc
            | count == 0 = acc
            | otherwise = rotations' s (pred count) ((rotate count s):acc) 

prettyrot :: String -> String
prettyrot = unwords . rotations

readInput :: IO [String]
readInput = do
    line <- getLine
    let count :: Int
        count = read line
    lines <- replicateM count $ do
        line <- getLine
        return line
    return lines

main :: IO ()
main = do
    n <- readLn :: IO Int
    lines <- replicateM n getLine
    let
        out = map prettyrot lines
    mapM_ putStrLn out
