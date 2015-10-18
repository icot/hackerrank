prefix :: Eq a => [a] -> [a] -> [a]
prefix a [] = []
prefix [] b = []
prefix (x:xs) (y:ys)
    | x == y = x : prefix xs ys
    | otherwise = []


remove p str = ...

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    let 
        p = prefix a b
    print p
