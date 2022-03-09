module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor 2 = 1
primeMayor coins
    |even coins          = 2
    |isPrime coins       = 1
    |isPrime (coins - 2) = 2
    |otherwise           = 3

isPrime :: Integer -> Bool 
isPrime n = and [n `mod` x /= 0 | x <- [2 .. floor (sqrt (fromInteger n))]]