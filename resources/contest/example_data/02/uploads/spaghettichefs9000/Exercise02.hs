module Exercise02 where

import Data.List (sort)

startupRevenue :: [Int] -> Int
startupRevenue xs = startupRevenue' (length xs) (sort xs)

startupRevenue' :: Int -> [Int] -> Int
startupRevenue' 0 _  = 0
startupRevenue' _ [] = 0
startupRevenue' n (x:xs) = max (n * x) (startupRevenue' (n - 1) xs)
