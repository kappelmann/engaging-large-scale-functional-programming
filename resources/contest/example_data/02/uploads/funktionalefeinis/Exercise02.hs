module Exercise02 where

import Data.List

startupRevenue :: [Int] -> Int
startupRevenue [] = 0
startupRevenue xs = startupRevSorted (zip [1..] $ sortBy (flip compare) xs) 0 0


startupRevSorted :: [(Int, Int)] -> Int -> Int -> Int
startupRevSorted [] acc price = acc
startupRevSorted ((i, a):xs) maxProfit price
    | i * a > maxProfit = startupRevSorted xs (i * a) a
    | otherwise = startupRevSorted xs maxProfit price