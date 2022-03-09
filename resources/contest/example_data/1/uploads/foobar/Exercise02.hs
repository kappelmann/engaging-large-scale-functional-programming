module Exercise02 where

import Data.List (nub, sort)

{- startupRevenue :: [Int] -> Int
startupRevenue cs = totalMoney (maximumBy (compare `on` (`totalMoney` cs)) cs) cs
    where
        gs = nub cs -}
{-
startupRevenue :: [Int] -> Int
startupRevenue cs = maximum total
    where
        gs = nub cs
        total = map (\x -> length (filter (>= x) cs) * x) gs
-}

startupRevenue :: [Int] -> Int
startupRevenue cs = getMax sorted 0 (length cs)
    where
        sorted = sort cs

getMax :: [Int] -> Int -> Int -> Int
getMax [] prev len = prev
getMax (x:xs) prev len
    | prev > next = getMax xs prev (len - 1) 
    | otherwise = getMax xs next (len - 1) 
    where
        next = x * len

{-
revenue :: [Int] -> Int -> Int -> Int
revenue (x:xs) prev len
    |
    where
        next = x * len
-}

totalMoney :: Int -> [Int] -> Int
totalMoney x cs = length (filter (>= x) cs) * x