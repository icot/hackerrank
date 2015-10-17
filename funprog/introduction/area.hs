import Text.Printf (printf)

f :: [Int] -> [Int] -> Double -> Double
f a b x = sum [ an * (x ** bn) | (an, bn) <- zip a' b' ]
    where
        a' = map fromIntegral a
        b' = map fromIntegral b

integrate :: Num a => (Double -> Double) -> Int -> Int -> Double
integrate fab l r = sum [ (fab c) * delta | c <-  [l',l'+delta..r'] ++ [r']]
    where
        delta = 0.001
        r' = fromIntegral r
        l' = fromIntegral l

volume :: (Double -> Double) -> Int -> Int -> Double
volume fab l r = pi * integrate r2 l r
    where 
        r2 y = (fab y) * (fab y)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [ integ, vol]
    where 
        f' = f a b
        vol = volume f' l r
        integ = integrate f' l r

main = do
      la <- getLine
      lb <- getLine
      lr <- getLine
      let 
          as = [ read a :: Int | a <- words la ]
          bs = [ read b :: Int | b <- words lb ]
          rs = [ read r :: Int | r <- words lr ]
          l = rs !! 0 
          r = rs !! 1
          result = solve l r as bs
      mapM_ (printf "%.1f\n") result

