module Exercise03 where

primeMayor :: Integer -> Integer
primeMayor 2 = 1
primeMayor a
    | even a = 2
    | isPrime a = 1
    | otherwise = if isPrime (a-2) then 2 else 3

isPrime :: Integer -> Bool
isPrime n
    | null ([x | x <- [2 .. (ceiling . sqrt . fromInteger) n + 1], mod n x == 0]) = True
    | otherwise = False
