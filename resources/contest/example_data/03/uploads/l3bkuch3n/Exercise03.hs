module Exercise03 where

primeMayor :: Integer -> Integer 
primeMayor n 
    | n <= 3  = 1
    | even  n = 2 -- Der Hammer ! find ich klasse!
    | isPrime n = 1
    -- ungerade und nicht prim
    | otherwise =  if isPrime (n-2) then 2 else 3

-- prime (n - last (takeWhile (\x -> x < n))) -> 1

-- takeLast :: (Integer -> Bool) -> [Integer] -> Int
-- takeLast f (x:y:ys)
--   | f y = takeLast (y:ys)
--   | 

-- VL
factors :: Integer -> [Integer]
factors n = [m | m <- [1 .. n], n `mod` m == 0]

prime :: Integer -> Bool
prime n = factors n == [1,n]

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n 
  | n < 2 =  False
  | n `mod` 2 == 0 = False
  | otherwise = aux 3 n
  

aux :: Integer -> Integer -> Bool
aux i n 
    | (i * i) <= n = if n `mod` i == 0 then False else  aux (i+2) n
    | otherwise = True
        