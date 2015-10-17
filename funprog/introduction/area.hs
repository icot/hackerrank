import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = []

-- F
--f a b x = sum [ (fromIntegral an) * ((fromIntegral x) ** (fromIntegral bn)) | (an, bn) <- zip a b ]
f a b x = sum [ an * (x ** bn) | (an, bn) <- zip a b ]
f' = f a b

-- Area Integral
delta = 0.001 

--integrate :: Num a => (Double -> Double) -> Int -> Int -> Double
integrate fab l r = sum [ (fab c) * delta | c <-  [l',l'+delta..r'] ++ [r']]
    where
        r' = fromIntegral r
        l' = fromIntegral l

-- volume
volume fab l r = pi * integrate r2 l r
    where 
        r2 y = (fab y) * (fab y)

-- Test
a = [1..5]
b = [6..10]
l = 1
r = 4

vol = volume f' l r
integ = integrate f' l r


--Input/Output.
--main :: IO ()
--main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
