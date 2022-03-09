module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor x 
    | x == 2 = 1
    | even x = 2
    | isPrimeFastByAaronCopyright x = 1
    | isPrimeFastByAaronCopyright (x-2) = 2
    |otherwise = 3
        where primes = dropWhile(<x-2) $ takeWhile (<=x) getPrimes


isPrimeFastByAaronCopyright :: Integer -> Bool
isPrimeFastByAaronCopyright p = all (\x -> mod p x/=0) $ takeWhile (\x -> p>=x^2) [2..]

isPrime :: [Integer] -> Integer -> Bool
isPrime (x:xs) i
    |x>i = False
    |x==i = True
    |otherwise = isPrime (filter (\p -> mod p x /= 0) xs) i

--isPrime :: Integer -> Bool
--isPrime x = x `elem` takeWhile (<=x) getPrimes

getPrimes :: [Integer]
getPrimes = 2:filter primeCheck [3..]
  where primeCheck n = all (\p -> mod n p/=0) (takeWhile (\x -> n >= x^2) getPrimes)