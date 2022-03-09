module Exercise03 where


primeMayor :: Integer -> Integer 
primeMayor inti 
    | inti == 2 = 1
    | even inti = 2
    | inti `elem` n = 1
    | otherwise = 3
        where
            n = takeWhile(<=inti) primes


primes :: [Integer]
primes = sieve[2..]

sieve :: Integral a => [a] -> [a]
sieve(p:xs) = p : sieve[ele|ele<-xs, ele `mod` p /= 0]