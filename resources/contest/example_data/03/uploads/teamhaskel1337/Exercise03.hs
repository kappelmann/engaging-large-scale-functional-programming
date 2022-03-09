module Exercise03 where

isPrime :: Integer -> Bool
isPrime x = not $ any (\k->(x `mod` k)==0) (filter (/= 2) [2..(ceiling $ sqrt $ fromInteger x)])

primeMayor :: Integer -> Integer 
primeMayor 0 = 0
primeMayor 2 = 1
primeMayor a
    | even a = 2
    | isPrime a = 1
    | isPrime (a-2) = 2
    | otherwise = 3