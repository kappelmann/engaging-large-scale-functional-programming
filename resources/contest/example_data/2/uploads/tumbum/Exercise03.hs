module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor n | even n = 2
             | primeTest n = 1
             | primeTest (n-2) = 2
             | otherwise = 3
                
primeTest :: Integer -> Bool
primeTest n = not $ any (\x -> n `mod` x == 0) [2..n`div`2]
