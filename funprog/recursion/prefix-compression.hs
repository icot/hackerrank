prefix :: Eq a => [a] -> [a] -> [a]
prefix a [] = []
prefix [] b = []
prefix (x:xs) (y:ys)
    | x == y = x : prefix xs ys
    | otherwise = []

remove p str = drop (length p) str

output :: [Char] -> [Char]
output str = l ++ " " ++ str
    where l = show (length(str))

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let 
        p = prefix x y
        x' = remove p x
        y' = remove p y
        out = [p, x', y'] 
    mapM_ putStrLn (map output out)
