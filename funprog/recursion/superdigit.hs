sumDigits :: Integer -> Integer
sumDigits n
    | n < 10 = n
    | otherwise = n `mod` 10 + sumDigits (n `div` 10)

sumd = sum . map (read . return) . show

superDigit :: Integer -> Integer
superDigit n
    | n < 10 = n
    | otherwise = superDigit (sumd n)

compose :: Int -> Int -> Integer
compose n k = read n'
    where n' = foldr (++) [] (map show (replicate k n))

main :: IO ()
main = do
    input <- getLine
    let 
        buf = [ (read i)::Int | i <- words input ]
        n'  = compose (head buf) (last buf)
        out = superDigit n'
    putStrLn (show out)
