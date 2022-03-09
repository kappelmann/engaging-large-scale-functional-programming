module Exercise02 where

import Data.List(sort)
import Test.QuickCheck

startupRevenue1 :: [Int] -> Int
startupRevenue1 []   = 0
startupRevenue1 list = maximum (map (getPrices sortedL) list)
                    where sortedL = sort list


getPrices :: [Int] -> Int -> Int 
getPrices [] _  = 0
getPrices (x:xs) price
    | x >= price = price * (length xs + 1)
    | otherwise = getPrices xs price

startupRevenue :: [Int] -> Int
startupRevenue [] = 0
startupRevenue list = maximum (count sorted (head sorted) (length list) 0)
                    where sorted = sort list


count :: [Int] -> Int -> Int -> Int -> [Int]
count [] _ _ _     = []
count (x:xs) price len nRem | price /= x    = (len * price) : count (x:xs) x (len - nRem) 0
                            | null xs       = [len * price]
                            | otherwise     = count xs price len (nRem + 1)

                   
prop_test :: [Int] -> Property
prop_test xs = not (elem 0 xs) ==> startupRevenue x2 == startupRevenue1 x2
     where x2 = [abs a | a <- xs]-- ???? ^^
