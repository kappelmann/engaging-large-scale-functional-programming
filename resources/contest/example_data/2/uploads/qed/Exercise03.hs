module Exercise03 where

isPrime :: Integer -> Bool
isPrime x | x > 1 = and [x `mod` y /= 0 | y <- [2..(floor (sqrt (fromIntegral x)))]]
isPrime _ = False

primeMayor :: Integer -> Integer
primeMayor income
    | income == 2 = 1
    | even income = 2
    | isPrime income = 1
    | isPrime (income - 2) = 2
    | otherwise = 3

