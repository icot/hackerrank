sumd = sum . map (read . return) . show

superDigit :: Integer -> Integer
superDigit n
    | n < 10 = n
    | otherwise = superDigit (sumd n)

main :: IO ()
main = do
    input <- getLine
    let 
        buf = [ (read i)::Integer | i <- words input ]
        n = head buf
        k = last buf
        n'  = if (n < 10) && (k == 1)
            then n
            else k * sumd n  
        out = superDigit n'
    putStrLn (show out)
