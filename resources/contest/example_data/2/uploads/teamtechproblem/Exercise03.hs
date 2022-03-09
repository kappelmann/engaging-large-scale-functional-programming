module Exercise03 where
isPrime::Integer -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..floor(sqrt (fromIntegral k))], k `mod` x == 0]

primeMayor :: Integer -> Integer 
primeMayor x 
        |x < 2 = 0
        |isPrime x = 1
        |even x = 2
        |otherwise = if isPrime (x-2) then 2 else 3
