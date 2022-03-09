module Exercise03 where

import Data.List

primeMayor :: Integer -> Integer
primeMayor n
    | even n = 2
    | prime n = 1
    | even n || test = 2
    | otherwise = 3
        where 
            test = or [ prime (n-p) | p <- dropWhile (< div n 2) primes']
            primes' = takeWhile (<=n) primes

-- adapted from Lecture slide 375
prime :: Integer -> Bool
prime n  = n == head (dropWhile (<n) primes)

primes :: [Integer]
primes  =  sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs)  = p : sieve [x | x <- xs, x `mod` p /= 0]
