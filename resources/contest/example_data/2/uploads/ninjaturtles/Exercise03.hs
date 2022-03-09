module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor n
    | n <= 2            = 1
    | even n            = 2
    | isPrimeUneven n   = 1
    | otherwise         = if isPrimeUneven (n - 2) then 2 else 3

factors :: Integer -> [Integer]
factors n = [m | m <- [1 .. n], n `mod` m == 0]

isPrime :: Integer -> Bool
isPrime n = factors n == [1,n]

isPrimeUneven :: Integer -> Bool
isPrimeUneven n = isPrimeUneven' n 3

isPrimeUneven' :: Integer -> Integer -> Bool
isPrimeUneven' n k
    | k^2 > n           = True
    | n `mod` k == 0    = False
    | otherwise         = isPrimeUneven' n (k + 2)

----------- Not needed

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]