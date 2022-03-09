module Exercise03 where

primeMayor :: Integer -> Integer
primeMayor 2 = 1
primeMayor a
    | even a            = 2
    | isPrime a 2       = 1
    | isPrime (a-2) 2   = 2
    | otherwise         = 3

isPrime :: Integer -> Integer -> Bool
isPrime n i
    | i * i > n         = True
    | n `mod` i == 0    = False
    | otherwise         = isPrime n (i+1)
