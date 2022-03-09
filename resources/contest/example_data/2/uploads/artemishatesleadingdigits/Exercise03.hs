module Exercise03 where

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = odd n && null [x | x <- [2 .. floor (sqrt (fromInteger n))], n `mod` x == 0]

primes :: Integer -> [Integer]
primes n = filter isPrime is
  where
    is = [2 .. (n - 1)]

canBe2 n = isPrime (n - 2)

primeMayor :: Integer -> Integer
primeMayor 2 = 1
primeMayor n
  | even n = 2
  | isPrime n = 1
  | canBe2 n = 2
  | otherwise = 3
