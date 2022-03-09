module Exercise03 where

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
isPrime n = go 2
  where go x
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = x `seq` go (x + 1)

primeMayor :: Integer -> Integer
primeMayor 2 = 1
primeMayor n
  | even n = 2
  | isPrime n = 1
  | isPrime $ n - 2 = 2
  | otherwise = 3
