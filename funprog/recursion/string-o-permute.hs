import Control.Monad

permute :: [a] -> [a]
permute [] = []
permute s = concat [ [ fst x, snd x] | x <- zip evens odds]
    where 
        pairs = zip s [1..]
        odds = [ fst t | t <- pairs, odd (snd t)]
        evens = [ fst t | t <- pairs, even (snd t)]

main :: IO ()       
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map permute str
  mapM_ putStrLn ans
