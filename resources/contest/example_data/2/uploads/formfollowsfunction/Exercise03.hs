module Exercise03 where

import Data.List

primeMayor :: Integer -> Integer 
primeMayor wage = if even wage then 2 else primeMayor' wage 0
    where
        primeMayor' :: Integer -> Integer -> Integer 
        primeMayor' 0 n = n
        primeMayor' 1 n = n+1
        primeMayor' wage n = primeMayor' (wage - last (takeWhile (<= wage) primes)) (n+1)
        


factors :: Integer -> [Integer]
factors n = [m | m <- [1..n], n `mod` m == 0]

prime :: Integer -> Bool
prime n = factors n == [1,n]

primes :: [Integer]
primes = [p | p <- [1..], prime p, p < 10^10]