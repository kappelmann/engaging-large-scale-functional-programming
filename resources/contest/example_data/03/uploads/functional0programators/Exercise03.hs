module Exercise03 where

primeMayor :: Integer -> Integer
primeMayor 0 = 0
primeMayor x | even x = 2
             | otherwise = primeMayorHelper x

primeMayorHelper :: Integer -> Integer
primeMayorHelper 0 = 0 
primeMayorHelper x = 1 + primeMayorHelper (x - findBiggestPrime x)  

findBiggestPrime :: Integer -> Integer
findBiggestPrime 1 = 1
findBiggestPrime p | isPrime p 2 (specialSqrt p) = p
                   | otherwise = findBiggestPrime (p-1)


isPrime :: Integer -> Integer -> Integer -> Bool
isPrime x c m | c >= m = True
              | (x `mod` c) == 0 = False
              | otherwise = isPrime x (c+1) m

specialSqrt :: Integer -> Integer
specialSqrt x = (floor . sqrt $ fromIntegral x) + 1