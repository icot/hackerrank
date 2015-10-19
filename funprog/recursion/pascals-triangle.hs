factorial :: Integer -> Integer
factorial n
    | n <= 1 = 1
    | otherwise = n * factorial (pred n)

pascal n k 
    | n == k = 1
    | n > k = factorial n `div` (( factorial k) * (factorial (n - k)))
    | otherwise = 0

triangle k = [ line n | n <- [0..k] ]
    where line n = [ pascal n k | k <- [0..n] ]

prettyOut triangle = [ prettyLine l | l <- triangle ]
    where prettyLine l = unwords [ show i | i <- l ]

main :: IO ()
main = do 
    k <- getLine
    let
        k' = (read k)::Integer
    mapM_ putStrLn (prettyOut (triangle k'))
