module Exercise02 where

import Data.List ( nub, sort ) 

startupRevenue :: [Int] -> Int
startupRevenue xs = helper xs 0 1
    --myMap
    --helper xs 0 1

myMap :: [Int] -> Int
myMap xs = maximum $ map (getPriceForM xs) (nub xs)

getPriceForM :: [Int] -> Int -> Int
getPriceForM xs m = len * m
    where len = filterAndLength xs 0 m
    --filterAndLength xs 0 m [] 
    --length (myFilter xs m) * m 

myFilter :: [Int] -> Int -> [Int]
myFilter xs m = filter (>= m) xs















filterAndLength :: [Int] -> Int -> Int -> Int
filterAndLength [] len m = len
filterAndLength (x:xs) len m
    | x >= m = filterAndLength xs (len + 1) m
    | otherwise = filterAndLength xs len m

myAvg :: [Int] -> Int
myAvg xs = div (sum xs) (length xs)


helper :: [Int] -> Int -> Int -> Int
helper xs currMax n
    | n == (maximum xs + 1) = currMax
    | currMax < this = helper xs this (n+1)
    | otherwise = helper xs currMax (n+1)
    where this = getPriceForM xs n  

median :: Ord a => [a] -> a
median xs = sort xs !! div (length xs) 2

alternative :: [Int] -> Int
alternative xs = getPriceForM xs (median xs)