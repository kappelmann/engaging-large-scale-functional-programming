{-# LANGUAGE FlexibleContexts #-}

module Exercise03 where

import Data.List
primeMayor :: Integer -> Integer
primeMayor n
  | isPrime n = 1
  | even n || isPrime (n-2) = 2
  | otherwise = 3

powMod' :: Integer -> Integer -> Integer -> Integer
{- (a^k) `mod` n -}
powMod' a 1 n = a `mod` n
powMod' a k n | odd k = (a * powMod' a (k-1) n) `mod` n
              | otherwise = powMod' ((a*a) `mod` n) (k`div`2) n

powMod a k n = powMod' (a`mod`n) k n

millerRabin :: Integer -> Integer -> Bool
millerRabin a n =1 == powMod a k n && (let i0 = powMod a d n
                                       in (i0 == 1 || i0 == k) || f i0 1)
        where k = n - 1
              (d,m) = div2 k
              div2 :: (Integral a) => a -> (a, a)
              div2 x | odd x = (x,0)
                     | otherwise = let (d,m) = div2 (x `div` 2)
                                   in (d, m+1)
              f x i | i <= m = let x' = (x*x) `mod` n
                               in (x' == k) || f x' (i+1)
                    | otherwise = False

-- ps sind die primen Prüflingen
isPrime' n ps = n `elem` ps || all (`millerRabin` n) ps
isPrime n = isPrime' n [2,7,61]

isqrt = floor . sqrt . fromIntegral


primesTo m = sieve [2..m]
             where
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []



