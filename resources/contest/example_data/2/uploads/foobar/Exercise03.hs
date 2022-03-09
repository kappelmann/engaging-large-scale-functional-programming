module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor n
    | n == 2 || n == 3 = 1
    | even n = 2
    | isPrime n = 1 
    | isPrime (n-2) = 2
    | otherwise = 3

isPrime :: Integer -> Bool 
isPrime n = null [t | t <- [2 .. floor $ sqrt $ fromIntegral n], (n `mod` t == 0)]

