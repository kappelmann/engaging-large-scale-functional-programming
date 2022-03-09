module Exercise03 where

-- inspired by google and hard thinking :D
primeMayor :: Integer -> Integer
primeMayor a
    | even a = 2
    | isPrime a = 1
    | isPrime (a-2) = 2
    | otherwise = 3


isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 2 = n == 2
  | otherwise = odd n && isPrime' 3
  where
    isPrime' factor
      | factor * factor > n = True
      | otherwise = n `rem` factor /= 0 && isPrime' (factor+2) 
