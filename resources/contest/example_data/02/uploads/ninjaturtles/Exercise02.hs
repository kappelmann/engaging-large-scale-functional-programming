module Exercise02 where

import Data.List(sort, maximum)

startupRevenue :: [Int] -> Int
startupRevenue [] = 0
startupRevenue xs = maxRev sorted 0 (length xs)
    where   sorted = sort xs

-- xs must be sorted
maxRev :: [Int] -> Int -> Int -> Int
maxRev [] currMax _         = currMax
maxRev (x:xs) currMax len   = maxRev xs newMax (len - 1) 
    where   newMax = max currMax (x*len)

-------------- Not needed


rev :: [Int] -> Int -> Int
rev xs m = m * length (filter (>= m) xs)
