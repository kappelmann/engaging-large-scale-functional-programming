module Exercise02 where

import Data.List

startupRevenueSorted :: Int -> [Int] -> [Int]
startupRevenueSorted _ [] = []
startupRevenueSorted n xs@(x:rs) = (n * x) : startupRevenueSorted (pred n) rs

startupRevenue :: [Int] -> Int
startupRevenue xs = maximum $ startupRevenueSorted (length xs) $ sort xs
