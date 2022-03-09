module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor income 
    | even income && income /= 2 = 2
    | isPrime income = 1
    | otherwise = 1 + primeMayor (primediff income)

isPrime :: Integer -> Bool
isPrime k 
    | k <= 2 = True
    | otherwise = null [x | x <- [2 .. floor(sqrt $ fromIntegral k)], k `mod` x == 0]

primediff :: Integer -> Integer
primediff k = head [x | x <- [1..], even x, isPrime $ k - x ]