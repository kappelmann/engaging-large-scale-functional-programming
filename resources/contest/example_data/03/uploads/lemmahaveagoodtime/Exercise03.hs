module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor a 
    | even a = 2
    | a == head (dropWhile (<a) primes) = 1
    | otherwise = prime' a a 0

-- prime' :: Integer -> Integer -> Integer -> Integer
-- prime' a n cnt 
--     | a == 2 = cnt + 1
--     | even a = cnt + 2
--     | a < 1 = cnt
--     | n > a = prime' a a cnt
--     | b = prime' new_a new_a (cnt+1) 
--     | not b = prime' a (n-2) cnt 
--         where b = n == head (dropWhile (<n) primes)
--               new_a = a-n

prime' :: Integer -> Integer -> Integer -> Integer
prime' a n cnt 
    | a == 2 = cnt + 1
    | even a = cnt + 2
    | b = prime' new_a new_a (cnt+1) 
    | not b = prime' a (n-2) cnt 
        where b = n == head (dropWhile (< n) primes)
              new_a = a-n


isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

primes = filter isPrime [2 .. ]

-- primes :: [Integer] 
-- primes  =  sieve [2..]

-- sieve :: [Integer] -> [Integer]
-- sieve (p:xs)  = p : sieve [x | x <- xs, odd x && x `mod` p /= 0]

-- prime' :: Integer -> Integer -> Integer -> Integer
-- prime' a n cnt 
--     | n < 3  = cnt
--     | modulo == 0 = prime' new_a new_n (cnt+1)
--     | modulo /= 0 = prime' a (n-1) cnt
--         where modulo = a `mod` n
--               new_a = a `div` n
--               new_n = floor (sqrt (fromIntegral new_a))



