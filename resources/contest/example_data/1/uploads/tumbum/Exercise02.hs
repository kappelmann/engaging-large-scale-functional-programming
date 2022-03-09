module Exercise02 where

import Data.List

startupRevenue :: [Int] -> Int
startupRevenue xs = maximum $ help (sort xs) (length xs +1)

help :: [Int] -> Int -> [Int]
help [] _ = []
help (x:xs) n = x * (n-1) : help xs (n-1)